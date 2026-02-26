//! Device orientation sensor handling.

use calloop::LoopHandle;
use tracing::info;

use crate::catacomb::Catacomb;
#[cfg(feature = "iio-sensor-proxy")]
use crate::orientation::iio_sensor_proxy::IioAccelerometer;
use crate::orientation::udev::UdevAccelerometer;

#[cfg(feature = "iio-sensor-proxy")]
mod iio_sensor_proxy;
mod udev;

pub trait AccelerometerSource {
    /// Stop listening for orientation changes.
    fn pause(&mut self);

    /// Continue listening for orientation changes.
    fn resume(&mut self);
}

/// Platform-independent accelerometer implementation.
pub enum Accelerometer {
    Udev(UdevAccelerometer),
    #[cfg(feature = "iio-sensor-proxy")]
    Iio(IioAccelerometer),
    Dummy,
}

impl Accelerometer {
    /// Create a new accelerometer handle.
    pub async fn new(event_loop: &LoopHandle<'static, Catacomb>) -> Self {
        match UdevAccelerometer::new(event_loop) {
            Some(accelerometer) => {
                info!("Using Udev accelerometer");
                Self::Udev(accelerometer)
            },
            None => {
                #[cfg(feature = "iio-sensor-proxy")]
                if let Some(accelerometer) = IioAccelerometer::new(event_loop).await {
                    info!("Using iio-sensor-proxy accelerometer");
                    return Self::Iio(accelerometer);
                }

                info!("No supported accelerometer found");
                Self::Dummy
            },
        }
    }
}

impl AccelerometerSource for Accelerometer {
    fn pause(&mut self) {
        match self {
            Accelerometer::Udev(accelerometer) => accelerometer.pause(),
            #[cfg(feature = "iio-sensor-proxy")]
            Accelerometer::Iio(accelerometer) => accelerometer.pause(),
            Accelerometer::Dummy => (),
        }
    }

    fn resume(&mut self) {
        match self {
            Accelerometer::Udev(accelerometer) => accelerometer.resume(),
            #[cfg(feature = "iio-sensor-proxy")]
            Accelerometer::Iio(accelerometer) => accelerometer.resume(),
            Accelerometer::Dummy => (),
        }
    }
}
