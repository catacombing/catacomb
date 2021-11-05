//! Wayland shells.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use smithay::backend::renderer;
use smithay::backend::renderer::gles2::Gles2Texture;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Display;
use smithay::utils::{Logical, Physical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, BufferAssignment, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
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
                    let catacomb = data.get::<Catacomb>().unwrap();
                    if let Some(wl_surface) = surface.get_surface() {
                        catacomb.keyboard.set_focus(Some(wl_surface), SERIAL_COUNTER.next_serial());
                    }

                    xdg_windows.borrow_mut().add(surface, &catacomb.output);
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

    // Find the window responsible for the surface.
    let catacomb = data.get::<Catacomb>().unwrap();
    let mut windows = catacomb.windows.borrow_mut();
    let mut window = match windows.find(&surface) {
        Some(window) => window,
        None => return,
    };

    // Handle surface buffer changes.
    let mut buffer_size = Rectangle::default();
    compositor::with_surface_tree_upward(
        &surface,
        Point::from((0, 0)),
        |_, data, location| {
            let mut location = *location;

            // Update buffer attached to the surface.
            data.data_map.insert_if_missing(|| RefCell::new(SurfaceBuffer::new()));
            let mut buffer = data.data_map.get::<RefCell<SurfaceBuffer>>().unwrap().borrow_mut();
            let mut attributes = data.cached_state.current::<SurfaceAttributes>();
            buffer.update_buffer(&mut attributes);

            // Update the window's bounding box based on each buffer's size and location.
            if data.role == Some("subsurface") {
                let subsurface = data.cached_state.current::<SubsurfaceCachedState>();
                location += subsurface.location;
            }
            buffer_size = buffer_size.merge(Rectangle::from_loc_and_size(location, buffer.size()));

            TraversalAction::DoChildren(location)
        },
        |_, _, _| (),
        |_, _, _| true,
    );
    window.buffer_size = buffer_size.size;

    if !window.initial_configure_sent {
        window.initial_configure_sent = true;
        window.reconfigure();
    }
}

/// Surface buffer cache.
pub struct SurfaceBuffer {
    pub texture: Option<Rc<Gles2Texture>>,
    pub buffer: Option<Buffer>,
    size: Size<i32, Physical>,
    scale: i32,
}

impl SurfaceBuffer {
    fn new() -> Self {
        Self {
            scale: 1,
            texture: Default::default(),
            buffer: Default::default(),
            size: Default::default(),
        }
    }

    /// Handle buffer creation/removal.
    fn update_buffer(&mut self, attributes: &mut SurfaceAttributes) {
        match attributes.buffer.take() {
            Some(BufferAssignment::NewBuffer { buffer, .. }) => {
                self.size = renderer::buffer_dimensions(&buffer).unwrap_or_default();
                self.scale = attributes.buffer_scale;
                self.buffer = Some(Buffer(buffer));
                self.texture = None;
            },
            Some(BufferAssignment::Removed) => *self = Self::new(),
            None => (),
        }
    }

    /// Returns the size of the buffer.
    pub fn size(&self) -> Size<i32, Logical> {
        self.size.to_logical(self.scale)
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
