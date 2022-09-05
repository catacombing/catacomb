//! Catacomb compositor interface.
//!
//! This library provides abstractions for interacting with Catacomb's external
//! interfaces from Wayland clients.

mod catacomb;
mod config;
mod daemon;
mod drawing;
mod geometry;
mod input;
mod ipc;
mod layer;
mod orientation;
mod output;
mod overview;
mod socket;
mod udev;
mod windows;

pub use ipc::{send_message, IpcMessage};

/// Main compositor entry point; exported to prevent unused warnings.
#[doc(hidden)]
pub use crate::udev::run;
