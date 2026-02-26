//! Udev device-based orientation sensor.

use std::f32;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;

use catacomb_ipc::Orientation;
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use tracing::error;
use udev::{Device, Enumerator};

use crate::catacomb::Catacomb;
use crate::geometry::{Matrix3x3, Vector, Vector3D};
use crate::orientation::AccelerometerSource;

/// Threshold value for portrait/landscape mode.
const THRESHOLD: f32 = 35.;

/// Deadzone in which orientation will be preserved despite exceeding threshold.
const DEADZONE: f32 = 5.;

/// Orientation change poll rate.
const POLL_RATE: Duration = Duration::from_millis(500);

/// Udev accelorometer device.
pub struct UdevAccelerometer {
    event_loop: LoopHandle<'static, Catacomb>,
    token: RegistrationToken,
}

impl UdevAccelerometer {
    pub fn new(event_loop: &LoopHandle<'static, Catacomb>) -> Option<Self> {
        let mut state = AccelerometerState::new()?;

        let token = event_loop
            .insert_source(Timer::immediate(), move |_, _, catacomb| {
                let fallback = state.last.unwrap_or(Orientation::Portrait);
                let orientation = state.orientation().unwrap_or(fallback);
                if Some(orientation) != state.last {
                    state.last = Some(orientation);
                    catacomb.handle_orientation(orientation);
                }

                TimeoutAction::ToDuration(POLL_RATE)
            })
            .ok()?;

        Some(Self { token, event_loop: event_loop.clone() })
    }
}

/// Accelerometer calloop state.
struct AccelerometerState {
    syspath: PathBuf,
    last: Option<Orientation>,
    accel_mount_matrix: Matrix3x3<f32>,
}

impl AccelerometerState {
    pub fn new() -> Option<Self> {
        let mut enumerator = Enumerator::new().ok()?;
        enumerator.match_is_initialized().ok()?;
        enumerator.match_subsystem("iio").ok()?;

        let mut devices = enumerator.scan_devices().ok()?;
        let accel = devices.find(|device| device.attribute_value("in_accel_x_raw").is_some())?;

        // Read the mount_matrix values.
        let accel_mount_matrix = udev_attribute::<Matrix3x3<f32>>(&accel, "in_mount_matrix")
            .or_else(|| udev_attribute::<Matrix3x3<f32>>(&accel, "in_accel_mount_matrix"))
            .or_else(|| udev_attribute::<Matrix3x3<f32>>(&accel, "mount_matrix"))?;

        let syspath = accel.syspath().to_path_buf();

        Some(Self { accel_mount_matrix, syspath, last: Default::default() })
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

impl AccelerometerSource for UdevAccelerometer {
    fn pause(&mut self) {
        let _ = self
            .event_loop
            .disable(&self.token)
            .inspect_err(|err| error!("Failed to pause Udev accelerometer: {err}"));
    }

    fn resume(&mut self) {
        let _ = self
            .event_loop
            .enable(&self.token)
            .inspect_err(|err| error!("Failed to resume Udev accelerometer: {err}"));
    }
}

/// Get a udev device attribute.
fn udev_attribute<T: FromStr>(device: &Device, attribute: &str) -> Option<T> {
    let value = device.attribute_value(attribute)?.to_string_lossy();
    T::from_str(&value).ok()
}
