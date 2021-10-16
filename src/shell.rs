//! Wayland shells.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Mutex;

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Display;
use smithay::utils::{Logical, Point, Size};
use smithay::wayland::compositor::{
    self, BufferAssignment, Damage, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
use smithay::wayland::shell::xdg::{
    self as xdg_shell, ToplevelSurface, XdgRequest, XdgToplevelSurfaceRoleAttributes,
};
use smithay::wayland::SERIAL_COUNTER;
use wayland_commons::filter::DispatchData;

use crate::catacomb::Catacomb;

/// Wayland shells.
pub struct Shells {
    pub windows: Rc<RefCell<Vec<Window>>>,
}

impl Shells {
    /// Initialize all available shells.
    pub fn new(display: &mut Display) -> Self {
        // Create the compositor and register a surface commit handler.
        compositor::compositor_init(display, surface_commit, None);

        let windows = Rc::new(RefCell::new(Vec::new()));

        let xdg_windows = windows.clone();
        let _ = xdg_shell::xdg_shell_init(
            display,
            move |event, mut data| match event {
                XdgRequest::NewToplevel { surface } => {
                    if let Some(wl_surface) = surface.get_surface() {
                        let catacomb = data.get::<Catacomb>().unwrap();
                        catacomb.keyboard.set_focus(Some(wl_surface), SERIAL_COUNTER.next_serial());
                    }

                    xdg_windows.borrow_mut().push(Window::new(surface));
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

    let catacomb = data.get::<Catacomb>().unwrap();
    if let Some(window) = catacomb.windows.borrow_mut().iter_mut().find(|window| {
        window.surface.get_surface().map_or(false, |window_surface| window_surface == &surface)
    }) {
        let initial_configure_sent = compositor::with_states(&surface, |state| {
            let attributes = state.data_map.get::<Mutex<XdgToplevelSurfaceRoleAttributes>>();
            attributes.unwrap().lock().unwrap().initial_configure_sent
        })
        .unwrap_or(true);

        // Set the initial window dimensions.
        if !initial_configure_sent {
            let output_size = catacomb.output.size();
            window.resize(output_size);
        }
    }
}

/// Wayland client window state.
pub struct Window {
    pub surface: ToplevelSurface,
}

impl Window {
    fn new(surface: ToplevelSurface) -> Self {
        Window { surface }
    }

    /// Send a frame request to the window.
    pub fn request_frame(&self, runtime: u32) {
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        compositor::with_surface_tree_downward(
            wl_surface,
            (),
            |_, _, _| TraversalAction::DoChildren(()),
            |_, surface_data, _| {
                let mut attributes = surface_data.cached_state.current::<SurfaceAttributes>();
                for callback in attributes.frame_callbacks.drain(..) {
                    callback.done(runtime);
                }
            },
            |_, _, _| true,
        );
    }

    /// Render this window's buffers.
    pub fn draw(&self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output_scale: f64) {
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        compositor::with_surface_tree_upward(
            wl_surface,
            Point::from((0, 0)),
            |_, surface_data, location| {
                let data = match surface_data.data_map.get::<RefCell<SurfaceBuffer>>() {
                    Some(data) => data,
                    None => return TraversalAction::SkipChildren,
                };
                let mut data = data.borrow_mut();

                // Use the subsurface's location as the origin for its children.
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    // TODO: Shorten this?
                    let subsurface_state =
                        surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += subsurface_state.location;
                }

                // Start rendering if the buffer is already imported.
                if data.texture.is_some() {
                    return TraversalAction::DoChildren(location);
                }

                // Import and cache the buffer.
                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                let attributes = surface_data.cached_state.current::<SurfaceAttributes>();
                let damage: Vec<_> = attributes
                    .damage
                    .iter()
                    .map(|damage| match damage {
                        Damage::Buffer(rect) => *rect,
                        Damage::Surface(rect) => rect.to_buffer(attributes.buffer_scale),
                    })
                    .collect();

                match renderer.import_buffer(buffer, Some(surface_data), &damage) {
                    Some(Ok(texture)) => {
                        if let Some(BufferType::Shm) = renderer::buffer_type(buffer) {
                            data.buffer = None;
                        }
                        data.texture = Some(texture);

                        TraversalAction::DoChildren(location)
                    },
                    _ => {
                        eprintln!("unable to import buffer");
                        data.buffer = None;

                        TraversalAction::SkipChildren
                    },
                }
            },
            |_, surface_data, location| {
                let data = match surface_data.data_map.get::<RefCell<SurfaceBuffer>>() {
                    Some(data) => data,
                    None => return,
                };
                let data = data.borrow_mut();

                let texture = match &data.texture {
                    Some(texture) => texture,
                    None => return,
                };

                // Apply subsurface offset to parent's origin.
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    // TODO: Shorten this?
                    let subsurface_state =
                        surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += subsurface_state.location;
                }

                let attributes = surface_data.cached_state.current::<SurfaceAttributes>();

                let _ = frame.render_texture_at(
                    texture,
                    location.to_f64().to_physical(output_scale).to_i32_round(),
                    attributes.buffer_scale,
                    output_scale,
                    Transform::Normal,
                    1.,
                );
            },
            |_, _, _| true,
        );
    }

    /// Change the window dimensions.
    pub fn resize(&mut self, size: Size<i32, Logical>) {
        let result = self.surface.with_pending_state(|state| {
            state.size = Some(size);
        });

        if result.is_ok() {
            self.surface.send_configure();
        }
    }
}

/// Surface buffer cache.
#[derive(Default)]
struct SurfaceBuffer {
    texture: Option<Gles2Texture>,
    buffer: Option<Buffer>,
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
struct Buffer(WlBuffer);

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
