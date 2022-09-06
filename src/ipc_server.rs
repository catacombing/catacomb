//! IPC socket server.

use std::error::Error;
use std::fs;
use std::io::{BufRead, BufReader};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;

use catacomb_ipc::{self, IpcMessage};
use smithay::reexports::calloop::LoopHandle;

use crate::catacomb::Catacomb;
use crate::socket::SocketSource;

/// Create an IPC socket.
pub fn spawn_ipc_socket(
    event_loop: LoopHandle<'static, Catacomb>,
    socket_name: &str,
) -> Result<PathBuf, Box<dyn Error>> {
    let socket_path = catacomb_ipc::socket_path(socket_name);

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
