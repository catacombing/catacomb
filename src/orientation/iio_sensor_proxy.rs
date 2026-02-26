//! iio-sensor-proxy DBus orientation handling.

use calloop::LoopHandle;
use calloop::channel::{self as calloop_channel, Event};
use futures_lite::stream::StreamExt;
use tokio::sync::mpsc::{self as tokio_mpsc, Sender};
use tokio::task;
use tracing::{error, info, warn};
use zbus::Connection;

use crate::catacomb::Catacomb;
use crate::dbus::iio_sensor_proxy::{self, SensorProxyProxy};
use crate::orientation::AccelerometerSource;

/// iio-sensor-proxy accelerometer.
pub struct IioAccelerometer {
    tx: Sender<bool>,
}

impl IioAccelerometer {
    pub async fn new(event_loop: &LoopHandle<'static, Catacomb>) -> Option<Self> {
        let connection = Connection::system()
            .await
            .inspect_err(|err| warn!("Failed to connect to system DBus: {err}"))
            .ok()?;
        let sensor_proxy = SensorProxyProxy::new(&connection)
            .await
            .inspect_err(|err| warn!("Failed to connect to iio-sensor-proxy DBus: {err}"))
            .ok()?;

        // Check whether accelerometer is present before trying anything.
        if sensor_proxy.has_accelerometer().await != Ok(true) {
            info!("No accelerometers available through iio-sensor-proxy");
            return None;
        }

        // Claim the accelerometer, to allow us to receive orientation changes.
        sensor_proxy
            .claim_accelerometer()
            .await
            .inspect_err(|err| error!("Failed to claim iio-sensor-proxy accelerometer: {err}"))
            .ok()?;

        let mut orientation_stream = sensor_proxy.receive_accelerometer_orientation_changed().await;
        let (calloop_tx, calloop_rx) = calloop_channel::channel();
        let (tx, mut rx) = tokio_mpsc::channel(5);
        let event_loop = event_loop.clone();

        // Listen for orientation changes in the calloop event loop.
        event_loop
            .insert_source(calloop_rx, |event, _, catacomb| {
                if let Event::Msg(orientation) = event {
                    catacomb.handle_orientation(orientation);
                }
            })
            .inspect_err(|err| error!("Failed to schedule iio-sensor-proxy listener: {err}"))
            .ok()?;

        // Spawn a tokio thread to listen for DBus events.
        task::spawn(async move {
            loop {
                tokio::select!(
                    // Process orientation changes.
                    Some(orientation) = orientation_stream.next() => {
                        let orientation = match orientation.get().await {
                            Ok(orientation) => iio_sensor_proxy::parse_orientation(&orientation),
                            Err(err) => {
                                error!("Failed to read orientation property: {err}");
                                continue;
                            },
                        };
                        if let Err(err) = calloop_tx.send(orientation) {
                            error!("Failed to send iio-sensor-proxy orientation change: {err}");
                        }
                    },
                    // Handle pause/resume requests.
                    Some(claim) = rx.recv() => {
                        if claim {
                            if let Err(err) = sensor_proxy.claim_accelerometer().await {
                                error!("Failed to acquire accelerometer claim: {err}")
                            }
                        } else {
                            if let Err(err) = sensor_proxy.release_accelerometer().await {
                                error!("Failed to release accelerometer claim: {err}")
                            }
                        }
                    },
                );
            }
        });

        Some(Self { tx })
    }
}

impl AccelerometerSource for IioAccelerometer {
    fn pause(&mut self) {
        let _ = self
            .tx
            .try_send(false)
            .inspect_err(|err| error!("Failed to pause iio-sensor-proxy accelerometer: {err}"));
    }

    fn resume(&mut self) {
        let _ = self
            .tx
            .try_send(true)
            .inspect_err(|err| error!("Failed to resume iio-sensor-proxy accelerometer: {err}"));
    }
}
