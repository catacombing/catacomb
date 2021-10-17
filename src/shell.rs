//! Wayland shells.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Mutex;

use smithay::backend::renderer::gles2::Gles2Texture;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Display;
use smithay::wayland::compositor::{self, BufferAssignment, SurfaceAttributes, TraversalAction};
use smithay::wayland::shell::xdg::{
    self as xdg_shell, XdgRequest, XdgToplevelSurfaceRoleAttributes,
};
use smithay::wayland::SERIAL_COUNTER;
use wayland_commons::filter::DispatchData;

use crate::catacomb::Catacomb;
use crate::window::{Window, WindowLocation, Windows};

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

                    // Set window tiling location.
                    let location = xdg_windows
                        .borrow()
                        .iter()
                        .find(|window: &&Window| window.location == WindowLocation::Primary)
                        .map_or(WindowLocation::Primary, |_| WindowLocation::Secondary);

                    xdg_windows.borrow_mut().push(Window::new(surface, location));
                },
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
            surface_data.data_map.insert_if_missing(|| RefCell::new(SurfaceBuffer::new()));
            let mut attributes = surface_data.cached_state.current::<SurfaceAttributes>();

            if let Some(assignment) = attributes.buffer.take() {
                let data = surface_data.data_map.get::<RefCell<SurfaceBuffer>>().unwrap();
                data.borrow_mut().update_buffer(assignment);
            }
        },
        |_, _, _| true,
    );

    // Find the window for this surface.
    let catacomb = data.get::<Catacomb>().unwrap();
    let mut windows = catacomb.windows.borrow_mut();
    let window = match windows.iter_mut().find(|window| {
        window.surface.get_surface().map_or(false, |window_surface| window_surface == &surface)
    }) {
        Some(window) => window,
        None => return,
    };

    let initial_configure_sent = compositor::with_states(&surface, |state| {
        let attributes = state.data_map.get::<Mutex<XdgToplevelSurfaceRoleAttributes>>();
        attributes.unwrap().lock().unwrap().initial_configure_sent
    })
    .unwrap_or(true);

    // Set the initial window dimensions.
    if !initial_configure_sent {
        let mut size = catacomb.output.size();
        if window.location == WindowLocation::Primary {
            // Sole primary windows take up all the space.
            window.resize(size);
        } else {
            // Take half the space plus fractionals for the secondary window.
            let height = size.h as f32 / 2.;
            size.h = height.ceil() as i32;
            window.resize(size);

            // Resize primary window to take the first half.
            size.h = height.floor() as i32;
            if let Some(window) =
                windows.iter_mut().find(|window| window.location == WindowLocation::Primary)
            {
                window.resize(size);
            }
        }
    }
}

/// Surface buffer cache.
#[derive(Default)]
pub struct SurfaceBuffer {
    pub texture: Option<Gles2Texture>,
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
