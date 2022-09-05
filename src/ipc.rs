//! IPC socket communication.

use std::error::Error;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::{env, fs, process};

use clap::Subcommand;
use serde::{Deserialize, Serialize};
use smithay::reexports::calloop::LoopHandle;

use crate::catacomb::Catacomb;
use crate::orientation::Orientation;
use crate::socket::SocketSource;

/// IPC message format.
#[derive(Subcommand, Deserialize, Serialize, Debug)]
pub enum IpcMessage {
    /// Screen rotation (un)locking.
    Orientation {
        /// Lock rotation in the specified orientation.
        #[clap(long, min_values = 0, conflicts_with = "unlock")]
        lock: Option<Orientation>,
        /// Clear screen rotation lock.
        #[clap(long)]
        unlock: bool,
    },
}

/// Create an IPC socket.
pub fn spawn_ipc_socket(
    event_loop: LoopHandle<'static, Catacomb>,
    socket_name: &str,
) -> Result<PathBuf, Box<dyn Error>> {
    let socket_path = socket_path(socket_name);

    // Try to delete the socket if it exists already.
    if socket_path.exists() {
        fs::remove_file(&socket_path)?;
    }

    // Spawn unix socket event source.
    let listener = UnixListener::bind(&socket_path)?;
    let socket = SocketSource::new(listener)?;

    // Add source to calloop loop.
    let mut message_buffer = String::new();
    event_loop.insert_source(socket, move |stream, _, catacomb| {
        handle_message(&mut message_buffer, stream, catacomb)
    })?;

    Ok(socket_path)
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

/// Handle IPC socket messages.
fn handle_message(buffer: &mut String, stream: UnixStream, catacomb: &mut Catacomb) {
    buffer.clear();

    // Read new content to buffer.
    let mut stream = BufReader::new(stream);
    if let Ok(0) | Err(_) = stream.read_line(buffer) {
        return;
    }

    // Read pending events on socket.
    let message: IpcMessage = match serde_json::from_str(buffer) {
        Ok(message) => message,
        Err(_) => return,
    };

    // Handle IPC events.
    match message {
        IpcMessage::Orientation { unlock: true, .. } => catacomb.windows.unlock_orientation(),
        IpcMessage::Orientation { lock: orientation, .. } => {
            catacomb.windows.lock_orientation(orientation);
        },
    }
}

/// Path for the IPC socket file.
fn socket_path(socket_name: &str) -> PathBuf {
    dirs::runtime_dir().unwrap_or_else(env::temp_dir).join(format!("catacomb-{socket_name}.sock"))
}
