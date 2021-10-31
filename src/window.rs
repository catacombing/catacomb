//! Wayland client.

use std::cell::{RefCell, RefMut};
use std::mem;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
use smithay::wayland::shell::xdg::ToplevelSurface;

use crate::output::Output;
use crate::shell::SurfaceBuffer;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_DURATION: Duration = Duration::from_millis(200);

/// Container tracking all known clients.
#[derive(Default)]
pub struct Windows {
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    windows: Vec<Rc<RefCell<Window>>>,
    transaction_start: Option<Instant>,
}

impl Windows {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        if self.primary.strong_count() > 0 && self.secondary.strong_count() == 0 {
            // Start a transaction for resizing the primary window.
            self.transaction_start.get_or_insert_with(Instant::now);
            self.primary.upgrade().unwrap().borrow_mut().frozen = true;
            window.borrow_mut().frozen = true;

            // Spawn as secondary if there's only one primary window.
            self.secondary = Rc::downgrade(&window);
        } else {
            // TODO: Flickers if there's already primary + secondary.
            //   -> Transition into primary/secondary must be transactional too.
            //
            // Spawn as maximized primary if empty or secondary already present.
            self.primary = Rc::downgrade(&window);
            self.secondary = Weak::new();
        }
        self.windows.push(window);
    }

    /// Find the window responsible for a specific surface.
    pub fn find(&mut self, wl_surface: &WlSurface) -> Option<RefMut<Window>> {
        self.windows.iter_mut().map(|window| window.borrow_mut()).find(|window| {
            window.surface.get_surface().map_or(false, |surface| surface == wl_surface)
        })
    }

    /// Execute a function for all visible windows.
    pub fn with_visible<F: FnMut(&mut Window)>(&mut self, mut fun: F) {
        for window in self.primary.upgrade().iter_mut().chain(&mut self.secondary.upgrade()) {
            fun(&mut window.borrow_mut());
        }
    }

    /// Refresh the client list.
    ///
    /// This function will remove all dead windows and update the remaining ones appropriately.
    pub fn refresh(&mut self, output_size: Size<i32, Logical>) {
        // Start a transaction if exactly one of two visible windows died.
        let mut transaction_start = self.transaction_start.take();
        let primary = self.primary.upgrade().filter(|primary| !primary.borrow().zombie);
        let secondary = self.secondary.upgrade().filter(|secondary| !secondary.borrow().zombie);
        if let (Some(primary), Some(secondary)) = (primary, secondary) {
            let mut primary = primary.borrow_mut();
            let mut secondary = secondary.borrow_mut();

            primary.zombie = !primary.surface.alive();
            secondary.zombie = !secondary.surface.alive();

            // Skip transaction if both windows died.
            if primary.zombie ^ secondary.zombie {
                transaction_start.get_or_insert_with(Instant::now);
                primary.frozen = true;
                secondary.frozen = true;
            }
        }
        self.transaction_start = transaction_start;

        // Remove dead windows.
        self.windows.retain(|window| {
            let window = window.borrow();
            window.frozen || window.surface.alive()
        });

        self.update_dimensions(output_size);
    }

    /// Update size and location of visible windows.
    pub fn update_dimensions(&mut self, output_size: Size<i32, Logical>) {
        let primary = self.primary.upgrade().filter(|primary| !primary.borrow().zombie);
        let secondary = self.secondary.upgrade().filter(|secondary| !secondary.borrow().zombie);
        match (primary, secondary) {
            (Some(primary), Some(secondary)) => {
                let mut primary = primary.borrow_mut();
                let mut secondary = secondary.borrow_mut();

                let height = output_size.h as f32 / 2.;
                primary.rectangle.loc = Point::from((0, 0));
                primary.resize((output_size.w, height.ceil() as i32));

                secondary.rectangle.loc = Point::from((0, height.ceil() as i32));
                secondary.resize((output_size.w, height.floor() as i32));
            },
            (Some(window), None) | (None, Some(window)) => {
                let mut window = window.borrow_mut();
                window.rectangle.loc = Point::from((0, 0));
                window.resize(output_size);
            },
            _ => (),
        }
    }

    /// Attempt to execute pending transactions.
    pub fn update_transaction(&mut self) {
        let transaction_start = match self.transaction_start {
            Some(start) => start,
            None => return,
        };

        // Check if the transaction requires updating.
        if Instant::now().duration_since(transaction_start) <= MAX_TRANSACTION_DURATION {
            // Check if all participants are ready.
            let finished = self.windows.iter().map(|window| window.borrow()).all(|window| {
                !window.frozen || window.zombie || window.buffer_size == window.rectangle.size
            });

            // Abort if the transaction is still pending.
            if !finished {
                return;
            }
        }

        // Execute the transaction.
        self.windows.retain(|window| {
            let mut window = window.borrow_mut();
            window.frozen = false;
            window.surface.alive()
        });
        self.transaction_start = None;

        // Reorder windows if primary died.
        if self.primary.strong_count() == 0 && self.secondary.strong_count() > 0 {
            mem::swap(&mut self.primary, &mut self.secondary);
        }
    }
}

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
struct Texture {
    location: Point<i32, Logical>,
    texture: Rc<Gles2Texture>,
    scale: i32,
}

impl Texture {
    fn new(texture: Rc<Gles2Texture>, location: Point<i32, Logical>, scale: i32) -> Self {
        Self { texture, location, scale }
    }
}

/// Wayland client window state.
pub struct Window {
    /// Current buffer dimensions.
    pub buffer_size: Size<i32, Logical>,

    /// Desired window dimensions.
    rectangle: Rectangle<i32, Logical>,

    /// Attached surface.
    surface: ToplevelSurface,

    /// Texture cache, for rendering dead windows.
    textures: Vec<Texture>,

    /// Freeze window updates to allow atomic upgrades.
    frozen: bool,

    /// Dead window kept around for the cached buffer.
    zombie: bool,
}

impl Window {
    pub fn new(surface: ToplevelSurface) -> Self {
        Window {
            rectangle: Rectangle::default(),
            buffer_size: Size::default(),
            textures: Vec::new(),
            frozen: false,
            zombie: false,
            surface,
        }
    }

    /// Send a frame request to the window.
    pub fn request_frame(&self, runtime: u32) {
        // Ensure there is a drawable surface present.
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

    /// Change the window dimensions.
    pub fn resize<S: Into<Size<i32, Logical>>>(&mut self, size: S) {
        // Prevent redundant configure events.
        let size = size.into();
        if self.rectangle.size == size {
            return;
        }

        let result = self.surface.with_pending_state(|state| {
            state.size = Some(size);
        });

        if result.is_ok() {
            self.surface.send_configure();
            self.rectangle.size = size;
        }
    }

    /// Render this window's buffers.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output: &Output) {
        // Skip updating windows during transactions.
        if !self.frozen {
            self.import_buffers(renderer);
        }

        for Texture { texture, location, scale } in &self.textures {
            let _ = frame.render_texture_at(
                texture,
                location.to_f64().to_physical(output.scale).to_i32_round(),
                *scale,
                output.scale,
                Transform::Normal,
                1.,
            );
        }
    }

    /// Import the buffers of all surfaces into the renderer.
    fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        // Ensure there is a drawable surface present.
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        self.textures.clear();

        compositor::with_surface_tree_upward(
            wl_surface,
            self.rectangle.loc,
            |_, surface_data, location| {
                let data = match surface_data.data_map.get::<RefCell<SurfaceBuffer>>() {
                    Some(data) => data,
                    None => return TraversalAction::SkipChildren,
                };
                let mut data = data.borrow_mut();

                // Use the subsurface's location as the origin for its children.
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    let subsurface = surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += subsurface.location;
                }

                let attributes = surface_data.cached_state.current::<SurfaceAttributes>();
                let scale = attributes.buffer_scale;

                // Skip surface if buffer was already imported.
                if let Some(texture) = &data.texture {
                    self.textures.push(Texture::new(texture.clone(), location, scale));
                    return TraversalAction::DoChildren(location);
                }

                // Import and cache the buffer.

                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                let damage: Vec<_> = attributes
                    .damage
                    .iter()
                    .map(|damage| match damage {
                        Damage::Buffer(rect) => *rect,
                        Damage::Surface(rect) => rect.to_buffer(scale),
                    })
                    .collect();

                match renderer.import_buffer(buffer, Some(surface_data), &damage) {
                    Some(Ok(texture)) => {
                        // Release SHM buffers after import.
                        if let Some(BufferType::Shm) = renderer::buffer_type(buffer) {
                            data.buffer = None;
                        }

                        // Update and cache the texture.
                        let texture = Rc::new(texture);
                        data.texture = Some(texture.clone());
                        self.textures.push(Texture::new(texture, location, scale));

                        TraversalAction::DoChildren(location)
                    },
                    _ => {
                        eprintln!("unable to import buffer");
                        data.buffer = None;

                        TraversalAction::SkipChildren
                    },
                }
            },
            |_, _, _| (),
            |_, _, _| true,
        );
    }
}
