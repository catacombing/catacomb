use clap::{self, Parser, Subcommand};

use crate::ipc::IpcMessage;

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

/// Command line arguments.
#[derive(Parser, Debug)]
#[clap(author, about, version)]
struct Options {
    #[clap(subcommand)]
    pub subcommands: Option<Subcommands>,
}

#[derive(Subcommand, Debug)]
pub enum Subcommands {
    /// Send IPC messages to Catacomb.
    #[clap(subcommand)]
    Msg(IpcMessage),
}

pub fn main() {
    match Options::parse().subcommands {
        Some(Subcommands::Msg(msg)) => ipc::send_message(msg).expect("send IPC message"),
        None => udev::run(),
    }
}
