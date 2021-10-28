//! Wayland shells.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use smithay::backend::renderer::gles2::Gles2Texture;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Display;
use smithay::wayland::compositor::{self, BufferAssignment, SurfaceAttributes, TraversalAction};
use smithay::wayland::shell::xdg::{self as xdg_shell, XdgRequest};
use smithay::wayland::SERIAL_COUNTER;
use wayland_commons::filter::DispatchData;

use crate::catacomb::Catacomb;
use crate::window::Windows;

/// Wayland shells.
pub struct Shells {
    pub windows: Rc<RefCell<Windows>>,
}

impl Shells {
    /// Initialize all available shells.
    pub fn new(display: &mut Display) -> Self {
        // Create the compositor and register a surface commit handler.
        compositor::compositor_init(display, surface_commit, None);

        let windows = Rc::new(RefCell::new(Windows::new()));

        let xdg_windows = windows.clone();
        let _ = xdg_shell::xdg_shell_init(
            display,
            move |event, mut data| match event {
                XdgRequest::NewToplevel { surface } => {
                    // Automatically focus new windows.
                    if let Some(wl_surface) = surface.get_surface() {
                        let catacomb = data.get::<Catacomb>().unwrap();
                        catacomb.keyboard.set_focus(Some(wl_surface), SERIAL_COUNTER.next_serial());
                    }

                    xdg_windows.borrow_mut().add(surface);
                },
                XdgRequest::NewClient { .. } | XdgRequest::AckConfigure { .. } => (),
                _ => eprintln!("UNHANDLED EVENT: {:?}", event),
            },
            None,
        );

        Self { windows }
    }
}

/// Handle a new surface commit.
fn surface_commit(surface: WlSurface, mut data: DispatchData) {
    if compositor::is_sync_subsurface(&surface) {
        return;
    }

    // Handle surface buffer changes.
    compositor::with_surface_tree_upward(
        &surface,
        (),
        |_, _, _| TraversalAction::DoChildren(()),
        |_, surface_data, _| {
            let mut attributes = surface_data.cached_state.current::<SurfaceAttributes>();
            if let Some(assignment) = attributes.buffer.take() {
                surface_data.data_map.insert_if_missing(|| RefCell::new(SurfaceBuffer::new()));
                let buffer = surface_data.data_map.get::<RefCell<SurfaceBuffer>>().unwrap();
                buffer.borrow_mut().update_buffer(assignment);
            }
        },
        |_, _, _| true,
    );

    let catacomb = data.get::<Catacomb>().unwrap();
    catacomb.windows.borrow_mut().update_dimensions(catacomb.output.size());
}

/// Surface buffer cache.
#[derive(Default)]
pub struct SurfaceBuffer {
    pub texture: Option<Rc<Gles2Texture>>,
    pub buffer: Option<Buffer>,
}

impl SurfaceBuffer {
    fn new() -> Self {
        Self::default()
    }

    /// Handle buffer creation/removal.
    fn update_buffer(&mut self, assignment: BufferAssignment) {
        self.buffer = match assignment {
            BufferAssignment::NewBuffer { buffer, .. } => Some(Buffer(buffer)),
            BufferAssignment::Removed => None,
        };
        self.texture = None;
    }
}

/// Container for automatically releasing a buffer on drop.
pub struct Buffer(WlBuffer);

impl Drop for Buffer {
    fn drop(&mut self) {
        self.0.release();
    }
}

impl Deref for Buffer {
    type Target = WlBuffer;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
