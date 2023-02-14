use std::ffi::OsStr;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::{io, ptr};

use catacomb_ipc::IpcMessage;
use clap::{self, Parser, Subcommand};

mod catacomb;
mod config;
mod drawing;
mod geometry;
mod input;
mod ipc_server;
mod layer;
mod orientation;
mod output;
mod overview;
mod protocols;
mod socket;
mod udev;
mod vibrate;
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
        Some(Subcommands::Msg(msg)) => catacomb_ipc::send_message(&msg).expect("send IPC message"),
        None => udev::run(),
    }
}

/// Spawn unsupervised daemons.
///
/// This will double-fork to avoid spawning zombies, but does not provide any
/// ability to retrieve the process output.
pub fn daemon<I, S>(program: S, args: I) -> io::Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut command = Command::new(program);
    command.args(args);
    command.stdin(Stdio::null());
    command.stdout(Stdio::null());
    command.stderr(Stdio::null());

    unsafe {
        command.pre_exec(|| {
            // Perform second fork.
            match libc::fork() {
                -1 => return Err(io::Error::last_os_error()),
                0 => (),
                _ => libc::_exit(0),
            }

            if libc::setsid() == -1 {
                return Err(io::Error::last_os_error());
            }

            // Reset signal handlers.
            let mut signal_set = std::mem::MaybeUninit::uninit();
            libc::sigemptyset(signal_set.as_mut_ptr());
            libc::sigprocmask(libc::SIG_SETMASK, signal_set.as_mut_ptr(), ptr::null_mut());

            Ok(())
        });
    }

    command.spawn()?.wait()?;

    Ok(())
}
