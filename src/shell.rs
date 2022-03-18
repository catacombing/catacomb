//! Wayland shells.

use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{DispatchData, Display};
use smithay::wayland::compositor;
use smithay::wayland::shell::wlr_layer::{self, LayerShellRequest};
use smithay::wayland::shell::xdg::{self as xdg_shell, XdgRequest};

use crate::catacomb::Catacomb;

/// Initialize all available shells.
pub fn init<B: 'static>(display: &mut Display) {
    // Create the compositor and register a surface commit handler.
    compositor::compositor_init(display, surface_commit::<B>, None);

    // XDG Shell.
    let _ = xdg_shell::xdg_shell_init(
        display,
        |event, mut data| match event {
            XdgRequest::NewToplevel { surface } => {
                let catacomb = data.get::<Catacomb<B>>().unwrap();
                catacomb.windows.add(surface, &catacomb.output);
            },
            XdgRequest::AckConfigure { surface, .. } => {
                // Request new frames after each resize.
                let catacomb = data.get::<Catacomb<B>>().unwrap();
                let runtime = catacomb.windows.runtime();
                if let Some(mut window) = catacomb.windows.find_xdg(&surface) {
                    window.request_frame(runtime);
                }
            },
            XdgRequest::NewPopup { surface, .. } => {
                let catacomb = data.get::<Catacomb<B>>().unwrap();
                catacomb.windows.add_popup(surface);
            },
            XdgRequest::Grab { .. } => (),
            XdgRequest::NewClient { .. } => (),
            _ => eprintln!("UNHANDLED EVENT: {event:?}"),
        },
        None,
    );

    // Layer shell.
    wlr_layer::wlr_layer_shell_init(
        display,
        |event, mut data| match event {
            LayerShellRequest::NewLayerSurface { surface, layer, .. } => {
                let catacomb = data.get::<Catacomb<B>>().unwrap();
                catacomb.windows.add_layer(layer, surface);
            },
            LayerShellRequest::AckConfigure { .. } => (),
        },
        None,
    );
}

/// Handle a new surface commit.
fn surface_commit<B: 'static>(surface: WlSurface, mut data: DispatchData) {
    if compositor::is_sync_subsurface(&surface) {
        return;
    }

    let catacomb = data.get::<Catacomb<B>>().unwrap();
    catacomb.windows.surface_commit(&surface, &mut catacomb.output);
}
