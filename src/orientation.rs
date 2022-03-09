//! Device orientation.

use std::f32;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;

use calloop::timer::Timer;
use calloop::LoopHandle;
use smithay::utils::Transform;
use udev::{Device, Enumerator};

use crate::catacomb::Catacomb;

/// Orientation change poll rate.
const POLL_RATE: Duration = Duration::from_millis(500);

/// Threshold value for portrait/landscape mode.
const THRESHOLD: f32 = 35.;

/// Deadzone in which orientation will be preserved despite exceeding threshold.
const DEADZONE: f32 = 5.;

pub trait AccelerometerSource {
    /// Subscribe to orientation change events.
    fn subscribe<'a, F: 'a, B>(self, loop_handle: LoopHandle<'a, Catacomb<B>>, fun: F)
    where
        F: FnMut(Orientation, &mut Catacomb<B>);
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
            .map(Accelerometer::Sensor)
            .unwrap_or(Accelerometer::Dummy(DummyAccelerometer))
    }
}

impl AccelerometerSource for Accelerometer {
    fn subscribe<'a, F: 'a, B>(self, loop_handle: LoopHandle<'a, Catacomb<B>>, fun: F)
    where
        F: FnMut(Orientation, &mut Catacomb<B>),
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
}

impl SensorAccelerometer {
    /// Attempt to find the accelerometer device.
    fn new() -> Option<Self> {
        let mut enumerator = Enumerator::new().ok()?;
        enumerator.match_is_initialized().ok()?;
        enumerator.match_subsystem("iio").ok()?;

        let mut devices = enumerator.scan_devices().ok()?;
        let accel = devices.find(|device| device.attribute_value("in_accel_x_raw").is_some());
        accel.map(|accel| Self { syspath: accel.syspath().to_path_buf(), last: None })
    }

    /// Check device orientation.
    ///
    /// This code was heavily inspired by iio-sensor-proxy:
    /// <https://gitlab.freedesktop.org/hadess/iio-sensor-proxy/-/blob/bba45607486acbe2f69df828692a9b1e8a649019/src/orientation.c#L64>
    fn orientation(&self) -> Option<Orientation> {
        // Read data from sensor.
        let device = Device::from_syspath(&self.syspath).ok()?;
        let mut x = udev_attribute::<i32>(&device, "in_accel_x_raw")? as f32;
        let mut y = udev_attribute::<i32>(&device, "in_accel_y_raw")? as f32;
        let mut z = udev_attribute::<i32>(&device, "in_accel_z_raw")? as f32;
        let scale: f32 = udev_attribute(&device, "in_accel_scale")?;

        // Apply scale to get to m/s², chen convert 1G ~= 256 for the algorithm to work properly.
        x = x * scale * 256. / 9.81;
        y = y * scale * 256. / 9.81;
        z = z * scale * 256. / 9.81;

        // Compute angle to landscape/portrait mode.
        //
        // This will return 90° for landscape/portrait when the modes are fully active and
        // transition to -90° for the inverse.
        let radians_to_degrees = 180. / f32::consts::PI;
        let portrait = (x.atan2((y * y + z * z).sqrt()) * radians_to_degrees).round();
        let landscape = (y.atan2((x * x + z * z).sqrt()) * radians_to_degrees).round();
        let portrait_abs = portrait.abs();
        let landscape_abs = landscape.abs();

        // Deadzone to avoid flickering between orientation.
        if portrait_abs + DEADZONE > THRESHOLD && landscape_abs + DEADZONE > THRESHOLD {
            return None;
        }

        let orientation = if portrait_abs > THRESHOLD {
            if portrait > 0. {
                Orientation::Portrait
            } else {
                Orientation::InversePortrait
            }
        } else if landscape_abs > THRESHOLD {
            if landscape > 0. {
                Orientation::InverseLandscape
            } else {
                Orientation::Landscape
            }
        } else {
            // No orientation is present when parallel to the ground.
            return None;
        };

        Some(orientation)
    }
}

impl AccelerometerSource for SensorAccelerometer {
    fn subscribe<'a, F: 'a, B>(mut self, loop_handle: LoopHandle<'a, Catacomb<B>>, mut fun: F)
    where
        F: FnMut(Orientation, &mut Catacomb<B>),
    {
        let timer = Timer::new().expect("create orientation timer");
        timer.handle().add_timeout(Duration::ZERO, ());
        loop_handle
            .insert_source(timer, move |_, handle, catacomb| {
                let fallback = self.last.unwrap_or(Orientation::Portrait);
                let orientation = self.orientation().unwrap_or(fallback);
                if Some(orientation) != self.last {
                    self.last = Some(orientation);
                    fun(orientation, catacomb);
                }

                handle.add_timeout(POLL_RATE, ());
            })
            .expect("insert orientation timer");
    }
}

/// Fake accelerometer device.
///
/// This will always return [`Orientation::Portrait`] and is used for devices which do not have an
/// accelerometer.
pub struct DummyAccelerometer;

impl AccelerometerSource for DummyAccelerometer {
    fn subscribe<'a, F: 'a, B>(self, loop_handle: LoopHandle<'a, Catacomb<B>>, mut fun: F)
    where
        F: FnMut(Orientation, &mut Catacomb<B>),
    {
        let timer = Timer::new().expect("create dummy orientation timer");
        timer.handle().add_timeout(Duration::ZERO, ());
        loop_handle
            .insert_source(timer, move |_, _, catacomb| fun(Orientation::Portrait, catacomb))
            .expect("insert dummy orientation timer");
    }
}

/// Device orientation.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Orientation {
    /// Portrait mode.
    Portrait,

    // Inverse portrait mode.
    InversePortrait,

    /// Landscape mode.
    Landscape,

    /// Inverse landscape mode.
    InverseLandscape,
}

impl Default for Orientation {
    fn default() -> Self {
        Orientation::Portrait
    }
}

impl Orientation {
    /// Display rendering transform for this orientation.
    pub fn transform(&self) -> Transform {
        match self {
            Self::Portrait => Transform::Normal,
            Self::InversePortrait => Transform::_180,
            Self::Landscape => Transform::_90,
            Self::InverseLandscape => Transform::_270,
        }
    }
}

/// Get a udev device attribute.
fn udev_attribute<T: FromStr>(device: &Device, attribute: &str) -> Option<T> {
    let value = device.attribute_value(attribute)?.to_string_lossy();
    T::from_str(&value).ok()
}
