//! IPC socket server.

use std::error::Error;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;

use catacomb_ipc::{AppIdMatcher, CliToggle, IpcMessage, Keysym, WindowScale};
use smithay::reexports::calloop::LoopHandle;
use tracing::{error, warn};

use crate::catacomb::Catacomb;
use crate::config::{GestureBinding, GestureBindingAction, KeyBinding};
use crate::socket::SocketSource;

/// Create an IPC socket.
pub fn spawn_ipc_socket(
    event_loop: &LoopHandle<'static, Catacomb>,
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
        handle_message(&mut message_buffer, stream, catacomb);
    })?;

    Ok(socket_path)
}

/// Handle IPC socket messages.
fn handle_message(buffer: &mut String, mut stream: UnixStream, catacomb: &mut Catacomb) {
    buffer.clear();

    // Read new content to buffer.
    let mut reader = BufReader::new(&stream);
    if let Ok(0) | Err(_) = reader.read_line(buffer) {
        return;
    }

    // Read pending events on socket.
    let message: IpcMessage = match serde_json::from_str(buffer) {
        Ok(message) => message,
        Err(_) => return,
    };

    // Handle IPC events.
    match message {
        IpcMessage::Orientation { unlock: true, .. } => catacomb.unlock_orientation(),
        IpcMessage::Orientation { lock: orientation, .. } => catacomb.lock_orientation(orientation),
        IpcMessage::Scale { scale, app_id: Some(app_id) } => {
            let app_id = match AppIdMatcher::try_from(app_id) {
                Ok(app_id) => app_id,
                Err(err) => {
                    warn!("ignoring invalid ipc message: scale has invalid App ID regex: {err}");
                    return;
                },
            };

            catacomb.windows.add_window_scale(app_id, scale);
        },
        IpcMessage::Scale { scale, app_id: None } => {
            let scale = match scale {
                WindowScale::Fixed(scale) => scale,
                scale => {
                    warn!("ignoring invalid ipc message: expected fixed scale, got {scale:?}");
                    return;
                },
            };

            catacomb.windows.set_scale(scale);
            catacomb.unstall();
        },
        IpcMessage::BindGesture { app_id, start, end, program, arguments } => {
            let app_id = match AppIdMatcher::try_from(app_id) {
                Ok(app_id) => app_id,
                Err(err) => {
                    warn!("ignoring invalid ipc message: binding has invalid App ID regex: {err}");
                    return;
                },
            };

            let action = GestureBindingAction::Cmd((program, arguments));
            let gesture = GestureBinding { app_id, start, end, action };
            catacomb.touch_state.user_gestures.push(gesture);
        },
        IpcMessage::BindGestureKey { app_id, start, end, mods, key } => {
            let app_id = match AppIdMatcher::try_from(app_id) {
                Ok(app_id) => app_id,
                Err(err) => {
                    warn!("ignoring invalid ipc message: binding has invalid App ID regex: {err}");
                    return;
                },
            };

            // Ignore custom keysyms like virtual keyboard enable/disable.
            let key = match key {
                Keysym::Xkb(key) => key,
                _ => {
                    warn!("ignoring invalid ipc message: gestures must be bound to real keysyms");
                    return;
                },
            };

            let action = GestureBindingAction::Key((key, mods.unwrap_or_default()));
            let gesture = GestureBinding { app_id, start, end, action };
            catacomb.touch_state.user_gestures.push(gesture);
        },
        IpcMessage::UnbindGesture { app_id, start, end } => {
            catacomb.touch_state.user_gestures.retain(|gesture| {
                gesture.app_id.base() != app_id || gesture.start != start || gesture.end != end
            });
        },
        IpcMessage::BindKey { app_id, mods, on_press, key, program, arguments } => {
            let app_id = match AppIdMatcher::try_from(app_id) {
                Ok(app_id) => app_id,
                Err(err) => {
                    warn!("ignoring invalid ipc message: binding has invalid App ID regex: {err}");
                    return;
                },
            };

            let binding = KeyBinding {
                on_press,
                app_id,
                program,
                arguments,
                key,
                mods: mods.unwrap_or_default(),
            };
            catacomb.key_bindings.push(binding);
        },
        IpcMessage::UnbindKey { app_id, mods, key } => {
            let mods = mods.unwrap_or_default();

            catacomb.key_bindings.retain(|binding| {
                binding.app_id.base() != app_id || binding.key != key || binding.mods != mods
            });
        },
        IpcMessage::Dpms { state: Some(state) } => {
            catacomb.set_display_status(state == CliToggle::On)
        },
        IpcMessage::Dpms { state: None } => {
            let state = if catacomb.display_on { CliToggle::On } else { CliToggle::Off };
            send_reply(&mut stream, &IpcMessage::DpmsReply { state });
        },
        IpcMessage::Cursor { state } => {
            catacomb.draw_cursor = state == CliToggle::On;
        },
        // Ignore IPC replies.
        IpcMessage::DpmsReply { .. } => (),
    }
}

/// Send IPC message reply.
fn send_reply(stream: &mut UnixStream, message: &IpcMessage) {
    if let Err(err) = send_reply_fallible(stream, message) {
        error!("Could not send IPC reply: {err}");
    }
}

/// Send IPC message reply, returning possible errors.
fn send_reply_fallible(
    stream: &mut UnixStream,
    message: &IpcMessage,
) -> Result<(), Box<dyn Error>> {
    let json = serde_json::to_string(&message)?;
    stream.write_all(json[..].as_bytes())?;
    stream.flush()?;
    Ok(())
}
