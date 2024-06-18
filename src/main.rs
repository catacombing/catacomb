use std::ffi::OsStr;
use std::fmt::Display;
use std::mem::MaybeUninit;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::{env, io, ptr};

use catacomb_ipc::{DpmsState, IpcMessage};
use clap::{Parser, Subcommand};
#[cfg(feature = "profiling")]
use profiling::puffin;
#[cfg(feature = "profiling")]
use puffin_http::Server;
use tracing::error;
use tracing_subscriber::{EnvFilter, FmtSubscriber};

mod catacomb;
mod config;
mod daemon;
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
    #[cfg(feature = "profiling")]
    let _server = {
        puffin::set_scopes_on(true);
        Server::new(&format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT)).unwrap()
    };

    // Setup logging.
    let directives = env::var("RUST_LOG").unwrap_or("warn,catacomb=info".into());
    let env_filter = EnvFilter::builder().parse_lossy(directives);
    FmtSubscriber::builder().with_env_filter(env_filter).with_line_number(true).init();

    match Options::parse().subcommands {
        Some(Subcommands::Msg(msg)) => match catacomb_ipc::send_message(&msg) {
            Err(err) => eprintln!("\x1b[31merror\x1b[0m: {err}"),
            Ok(Some(IpcMessage::DpmsReply { state: DpmsState::On })) => println!("on"),
            Ok(Some(IpcMessage::DpmsReply { state: DpmsState::Off })) => println!("off"),
            Ok(_) => (),
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
