//! DBus system interfaces.

use std::error::Error;

use tokio::runtime::Builder;
use zbus::zvariant::OwnedFd;
use zbus::Connection;

use crate::dbus::logind::ManagerProxy;

#[allow(clippy::all)]
mod logind;

/// Register an idle inhibitor.
pub fn inhibit(what: &str, who: &str, why: &str, mode: &str) -> Result<OwnedFd, Box<dyn Error>> {
    let inhibitor = Builder::new_current_thread()
        .enable_all()
        .build()?
        .block_on(inhibit_async(what, who, why, mode))?;
    Ok(inhibitor)
}

/// Async handler for inhibiting the power button.
async fn inhibit_async(what: &str, who: &str, why: &str, mode: &str) -> zbus::Result<OwnedFd> {
    let connection = Connection::system().await?;
    let logind = ManagerProxy::new(&connection).await?;
    let inhibitor = logind.inhibit(what, who, why, mode).await?;
    Ok(inhibitor)
}
