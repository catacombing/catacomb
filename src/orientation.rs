//! Device orientation.

use std::f32;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;

// Orientation is in IPC for our lib target, reexport here for a more sensible path.
pub use catacomb_ipc::Orientation;
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use udev::{Device, Enumerator};

use crate::catacomb::Catacomb;
use crate::geometry::{Matrix3x3, Vector, Vector3D};

/// Orientation change poll rate.
const POLL_RATE: Duration = Duration::from_millis(500);

/// Threshold value for portrait/landscape mode.
const THRESHOLD: f32 = 35.;

/// Deadzone in which orientation will be preserved despite exceeding threshold.
const DEADZONE: f32 = 5.;

pub trait AccelerometerSource {
    /// Subscribe to orientation change events.
    fn subscribe<'a, F>(self, loop_handle: &LoopHandle<'a, Catacomb>, fun: F) -> RegistrationToken
    where
        F: FnMut(Orientation, &mut Catacomb) + 'a;
}

/// Platform-independent accelerometer implementation.
pub enum Accelerometer {
    Sensor(SensorAccelerometer),
    Dummy(DummyAccelerometer),
}

impl Accelerometer {
    /// Create a new accelerometer handle.
    pub fn new() -> Self {
        SensorAccelerometer::new()
            .map_or(Accelerometer::Dummy(DummyAccelerometer), Accelerometer::Sensor)
    }
}

impl AccelerometerSource for Accelerometer {
    fn subscribe<'a, F>(self, loop_handle: &LoopHandle<'a, Catacomb>, fun: F) -> RegistrationToken
    where
        F: FnMut(Orientation, &mut Catacomb) + 'a,
    {
        match self {
            Accelerometer::Sensor(accelerometer) => accelerometer.subscribe(loop_handle, fun),
            Accelerometer::Dummy(accelerometer) => accelerometer.subscribe(loop_handle, fun),
        }
    }
}

/// Real accelorometer device.
pub struct SensorAccelerometer {
    syspath: PathBuf,
    last: Option<Orientation>,
    accel_mount_matrix: Matrix3x3<f32>,
}

impl SensorAccelerometer {
    /// Attempt to find the accelerometer device.
    fn new() -> Option<Self> {
        let mut enumerator = Enumerator::new().ok()?;
        enumerator.match_is_initialized().ok()?;
        enumerator.match_subsystem("iio").ok()?;

        let mut devices = enumerator.scan_devices().ok()?;
        let accel = devices.find(|device| device.attribute_value("in_accel_x_raw").is_some())?;

        // Read the mount_matrix values.
        let accel_mount_matrix = udev_attribute::<Matrix3x3<f32>>(&accel, "in_mount_matrix")
            .or_else(|| udev_attribute::<Matrix3x3<f32>>(&accel, "in_accel_mount_matrix"))?;

        let syspath = accel.syspath().to_path_buf();

        Some(Self { syspath, last: None, accel_mount_matrix })
    }

    /// Check device orientation.
    ///
    /// This code was heavily inspired by iio-sensor-proxy:
    /// <https://gitlab.freedesktop.org/hadess/iio-sensor-proxy/-/blob/bba45607486acbe2f69df828692a9b1e8a649019/src/orientation.c#L64>
    fn orientation(&self) -> Option<Orientation> {
        // Read data from sensor.
        let device = Device::from_syspath(&self.syspath).ok()?;

        let x = udev_attribute::<i32>(&device, "in_accel_x_raw")? as f32;
        let y = udev_attribute::<i32>(&device, "in_accel_y_raw")? as f32;
        let z = udev_attribute::<i32>(&device, "in_accel_z_raw")? as f32;

        // Apply acceleration matrix.
        let accel_point = &self.accel_mount_matrix * Vector3D::new(x, y, z);

        let scale = udev_attribute::<f32>(&device, "in_accel_scale")? as f64;

        // Apply scale to get to m/s², then convert 1G ~= 256 for the algorithm to work.
        let Vector3D { x, y, z } = accel_point.scale(scale * 256. / 9.81);

        // Compute angle to landscape/portrait mode.
        //
        // This will return the angle between the orientation axis and the
        // position. So for example when the portrait_angle is zero it means
        // that the device is vertical, however to determine whether it's
        // upside down or not we must look at the angle to landscape.
        let radians_to_degrees = 180. / f32::consts::PI;
        let portrait_angle = (x.atan2((y * y + z * z).sqrt()) * radians_to_degrees).round();
        let landscape_angle = (y.atan2((x * x + z * z).sqrt()) * radians_to_degrees).round();

        let portrait_angle_abs = portrait_angle.abs();
        let landscape_angle_abs = landscape_angle.abs();

        // Deadzone to avoid flickering between orientation.
        if portrait_angle_abs + DEADZONE > THRESHOLD && landscape_angle_abs + DEADZONE > THRESHOLD {
            return None;
        }

        // Check if we're far enough from portrait axis to pick landscape mode.
        let orientation = if portrait_angle_abs > THRESHOLD {
            if portrait_angle > 0. { Orientation::InverseLandscape } else { Orientation::Landscape }
        // Ditto for landspace axis.
        } else if landscape_angle_abs > THRESHOLD {
            if landscape_angle > 0. { Orientation::InversePortrait } else { Orientation::Portrait }
        } else {
            // No orientation is present when parallel to the ground.
            return None;
        };

        Some(orientation)
    }
}

impl AccelerometerSource for SensorAccelerometer {
    fn subscribe<'a, F>(
        mut self,
        loop_handle: &LoopHandle<'a, Catacomb>,
        mut fun: F,
    ) -> RegistrationToken
    where
        F: FnMut(Orientation, &mut Catacomb) + 'a,
    {
        loop_handle
            .insert_source(Timer::immediate(), move |_, _, catacomb| {
                let fallback = self.last.unwrap_or(Orientation::Portrait);
                let orientation = self.orientation().unwrap_or(fallback);
                if Some(orientation) != self.last {
                    self.last = Some(orientation);
                    fun(orientation, catacomb);
                }

                TimeoutAction::ToDuration(POLL_RATE)
            })
            .expect("insert orientation timer")
    }
}

/// Fake accelerometer device.
///
/// This will always return [`Orientation::Portrait`] and is used for devices
/// which do not have an accelerometer.
pub struct DummyAccelerometer;

impl AccelerometerSource for DummyAccelerometer {
    fn subscribe<'a, F>(
        self,
        loop_handle: &LoopHandle<'a, Catacomb>,
        mut fun: F,
    ) -> RegistrationToken
    where
        F: FnMut(Orientation, &mut Catacomb) + 'a,
    {
        loop_handle
            .insert_source(Timer::immediate(), move |_, _, catacomb| {
                fun(Orientation::Portrait, catacomb);
                TimeoutAction::Drop
            })
            .expect("insert dummy orientation timer")
    }
}

/// Get a udev device attribute.
fn udev_attribute<T: FromStr>(device: &Device, attribute: &str) -> Option<T> {
    let value = device.attribute_value(attribute)?.to_string_lossy();
    T::from_str(&value).ok()
}
