//! DBus system interfaces.

use std::error::Error;
use std::thread;

use smithay::reexports::calloop::channel::{self, Channel, Sender};
use tokio::runtime::Builder;
use zbus::export::futures_util::stream::StreamExt;
use zbus::Connection;

use crate::dbus::logind::ManagerProxy;

#[allow(clippy::all)]
mod logind;

/// DBus signal events.
pub enum DBusEvent {
    /// System resumed from sleep.
    Unsuspend,
}

/// Spawn the DBus signal event loop.
pub fn dbus_listen() -> Result<Channel<DBusEvent>, Box<dyn Error>> {
    let (tx, rx) = channel::channel();
    thread::spawn(|| {
        Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("create tokio runtime")
            .block_on(dbus_signal_handler(tx))
            .expect("execute tokio runtime");
    });
    Ok(rx)
}

/// Suspend the system.
pub fn suspend() -> Result<(), Box<dyn Error>> {
    Builder::new_current_thread().enable_all().build()?.block_on(suspend_async())?;
    Ok(())
}

/// Async handler for the suspend call.
async fn suspend_async() -> zbus::Result<()> {
    let connection = Connection::system().await?;
    let logind = ManagerProxy::new(&connection).await?;
    logind.suspend(false).await
}

/// Async DBus signal event loop.
async fn dbus_signal_handler(tx: Sender<DBusEvent>) -> zbus::Result<()> {
    let connection = Connection::system().await?;
    let logind = ManagerProxy::new(&connection).await?;
    let mut suspend_stream = logind.receive_prepare_for_sleep().await?;

    while let Some(suspend_event) = suspend_stream.next().await {
        let suspend_end = !suspend_event.body::<bool>()?;
        if suspend_end {
            let tx_result = tx.send(DBusEvent::Unsuspend);
            if tx_result.is_err() {
                break;
            }
        }
    }

    Ok(())
}
