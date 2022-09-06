//! Catacomb compositor interface.
//!
//! This library provides abstractions for interacting with Catacomb's external
//! interfaces from Wayland clients.

//! IPC socket communication.

use std::error::Error;
use std::io::Write;
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::str::FromStr;
use std::{env, process};

#[cfg(feature = "clap")]
use clap::Subcommand;
use serde::{Deserialize, Serialize};
#[cfg(feature = "smithay")]
use smithay::utils::Transform;

/// IPC message format.
#[cfg_attr(feature = "clap", derive(Subcommand))]
#[derive(Deserialize, Serialize, Debug)]
pub enum IpcMessage {
    /// Screen rotation (un)locking.
    Orientation {
        /// Lock rotation in the specified orientation.
        #[cfg_attr(feature = "clap", clap(long, min_values = 0, conflicts_with = "unlock"))]
        lock: Option<Orientation>,
        /// Clear screen rotation lock.
        #[cfg_attr(feature = "clap", clap(long))]
        unlock: bool,
    },
}

/// Device orientation.
#[derive(Deserialize, Serialize, PartialEq, Eq, Copy, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
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

impl FromStr for Orientation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "portrait" => Ok(Self::Portrait),
            "inverse-portrait" => Ok(Self::InversePortrait),
            "landscape" => Ok(Self::Landscape),
            "inverse-landscape" => Ok(Self::InverseLandscape),
            _ => Err(format!(
                "Got {s:?}, expected one of portrait, inverse-portrait, landscape, or \
                 inverse-landscape"
            )),
        }
    }
}

#[cfg(feature = "smithay")]
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

/// Send a message to the Catacomb IPC socket.
pub fn send_message(message: IpcMessage) -> Result<(), Box<dyn Error>> {
    let socket_name = match env::var("WAYLAND_DISPLAY") {
        Ok(socket_name) => socket_name,
        Err(_) => {
            eprintln!("Error: WAYLAND_DISPLAY is not set");
            process::exit(101);
        },
    };

    let socket_path = socket_path(&socket_name);

    // Ensure Catacomb's IPC listener is running.
    if !socket_path.exists() {
        eprintln!("Error: IPC socket not found, ensure Catacomb is running");
        process::exit(102);
    }

    let mut socket = UnixStream::connect(&socket_path)?;

    let message = serde_json::to_string(&message)?;
    socket.write_all(message[..].as_bytes())?;
    let _ = socket.flush();

    Ok(())
}

/// Path for the IPC socket file.
pub fn socket_path(socket_name: &str) -> PathBuf {
    dirs::runtime_dir().unwrap_or_else(env::temp_dir).join(format!("catacomb-{socket_name}.sock"))
}
