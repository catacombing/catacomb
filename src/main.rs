use std::ffi::OsStr;
use std::fmt::Display;
use std::mem::MaybeUninit;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::{env, io, ptr};

use catacomb_ipc::IpcMessage;
use clap::{self, Parser, Subcommand};
use tracing::error;
use tracing_log::LogTracer;
use tracing_subscriber::{EnvFilter, FmtSubscriber};

mod catacomb;
mod config;
mod daemon;
mod dbus;
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
#[clap(author, about, version, max_term_width = 80)]
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
    // Try to initialize log-compatibility shim.
    let _ = LogTracer::init();

    // Set default level to WARN with Catacomb itself at INFO.
    let mut directives = String::from("warn,catacomb=info");

    // Override with `RUST_LOG` env variable.
    if let Ok(env_directives) = env::var("RUST_LOG") {
        directives = env_directives;
    }

    // Setup tracing.
    let env_filter = EnvFilter::builder().parse_lossy(directives);
    let subscriber =
        FmtSubscriber::builder().with_env_filter(env_filter).with_line_number(true).finish();
    let _ = tracing::subscriber::set_global_default(subscriber);

    match Options::parse().subcommands {
        Some(Subcommands::Msg(msg)) => {
            if let Err(err) = catacomb_ipc::send_message(&msg) {
                eprintln!("\x1b[31merror\x1b[0m: {err}");
            }
        },
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
            let mut signal_set = MaybeUninit::uninit();
            libc::sigemptyset(signal_set.as_mut_ptr());
            libc::sigprocmask(libc::SIG_SETMASK, signal_set.as_mut_ptr(), ptr::null_mut());

            Ok(())
        });
    }

    command.spawn()?.wait()?;

    Ok(())
}

/// Log an error, ignoring success.
pub fn trace_error<T, E: Display>(result: Result<T, E>) {
    if let Err(err) = &result {
        error!("{err}");
    }
}
