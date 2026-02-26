//! iio-sensor-proxy DBus interface.

use catacomb_ipc::Orientation;
use zbus::proxy;

#[proxy(
    interface = "net.hadess.SensorProxy",
    default_service = "net.hadess.SensorProxy",
    default_path = "/net/hadess/SensorProxy"
)]
pub trait SensorProxy {
    fn claim_accelerometer(&self) -> zbus::Result<()>;

    fn release_accelerometer(&self) -> zbus::Result<()>;

    #[zbus(property)]
    fn accelerometer_orientation(&self) -> zbus::Result<String>;

    #[zbus(property)]
    fn accelerometer_tilt(&self) -> zbus::Result<String>;

    #[zbus(property)]
    fn has_accelerometer(&self) -> zbus::Result<bool>;
}

pub fn parse_orientation(orientation: &str) -> Orientation {
    match orientation {
        "bottom-up" => Orientation::InversePortrait,
        "left-up" => Orientation::InverseLandscape,
        "right-up" => Orientation::Landscape,
        _ => Orientation::Portrait,
    }
}
