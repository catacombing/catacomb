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
#[derive(Default, Debug)]
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
    pub fn add(&mut self, surface: ToplevelSurface, output_size: Size<i32, Logical>) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        if self.primary.strong_count() > 0 && self.secondary.strong_count() == 0 {
            self.set_secondary(output_size, &window);
        } else {
            self.set_primary(output_size, &window);
            self.set_secondary(output_size, None);
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
        // Remove dead visible windows.
        if self.primary.upgrade().map_or(false, |primary| !primary.borrow().surface.alive()) {
            self.set_primary(output_size, None);
        }
        if self.secondary.upgrade().map_or(false, |secondary| !secondary.borrow().surface.alive()) {
            self.set_secondary(output_size, None);
        }

        // Remove dead windows.
        self.windows.retain(|window| window.borrow().surface.alive());
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
            let finished = self
                .windows
                .iter()
                .map(|window| window.borrow())
                .all(|window| !window.frozen || window.buffer_size == window.rectangle.size);

            // Abort if the transaction is still pending.
            if !finished {
                return;
            }
        }

        // Execute the transaction.
        for window in &self.windows {
            window.borrow_mut().frozen = false;
        }
        self.transaction_start = None;
    }

    /// Change the primary window.
    fn set_primary<'a>(
        &mut self,
        mut output_size: Size<i32, Logical>,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
    ) {
        // Fallback to secondary if primary is removed.
        let window = window.into().cloned().or_else(|| mem::take(&mut self.secondary).upgrade());
        let window = match window {
            Some(window) => window,
            None => {
                self.primary = Weak::new();
                return;
            },
        };

        let mut window_mut = window.borrow_mut();

        // Persist the old buffer until redraw.
        if let Some(primary) = self.primary.upgrade() {
            self.transaction_start.get_or_insert_with(Instant::now);
            window_mut.textures.append(&mut primary.borrow_mut().textures);
            window_mut.frozen = true;
        }

        // Set target window dimensions.
        if self.secondary.strong_count() > 0 {
            output_size.h = (output_size.h + 1) / 2;
        }
        window_mut.rectangle.loc = Point::from((0, 0));
        window_mut.resize(output_size);

        self.primary = Rc::downgrade(&window);
    }

    /// Change the secondary window.
    fn set_secondary<'a>(
        &mut self,
        mut output_size: Size<i32, Logical>,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
    ) {
        // Set initial size and frozen buffers for new secondary window.
        let window = window.into();
        if let Some(mut window) = window.map(|window| window.borrow_mut()) {
            if let Some(secondary) = self.secondary.upgrade() {
                mem::swap(&mut window.textures, &mut secondary.borrow_mut().textures);
            }
            let height = output_size.h as f32 / 2.;
            output_size.h = height.ceil() as i32;
            window.rectangle.loc = Point::from((0, output_size.h));
            window.resize(Size::from((output_size.w, height.floor() as i32)));
            window.frozen = true;
        }

        // Update secondary window size and frozen buffers.
        if let Some(mut primary) = self.primary.upgrade().as_ref().map(|p| p.borrow_mut()) {
            if let Some(secondary) = self.secondary.upgrade().filter(|_| window.is_none()) {
                primary.textures.append(&mut secondary.borrow_mut().textures);
            }
            primary.resize(output_size);
            primary.frozen = true;
        }

        // Replace window and start transaction for updates.
        self.secondary = window.map(Rc::downgrade).unwrap_or_default();
        self.transaction_start.get_or_insert_with(Instant::now);
    }
}

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
#[derive(Debug)]
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
#[derive(Debug)]
pub struct Window {
    /// Current buffer dimensions.
    pub buffer_size: Size<i32, Logical>,

    /// Initial size configure status.
    pub initial_configure_sent: bool,

    /// Desired window dimensions.
    rectangle: Rectangle<i32, Logical>,

    /// Attached surface.
    surface: ToplevelSurface,

    /// Texture cache, for rendering dead windows.
    textures: Vec<Texture>,

    /// Freeze window updates to allow atomic upgrades.
    frozen: bool,
}

impl Window {
    pub fn new(surface: ToplevelSurface) -> Self {
        Window {
            surface,
            initial_configure_sent: Default::default(),
            buffer_size: Default::default(),
            rectangle: Default::default(),
            textures: Default::default(),
            frozen: Default::default(),
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
        let old_size = mem::replace(&mut self.rectangle.size, size.into());
        if self.initial_configure_sent && self.rectangle.size != old_size {
            self.reconfigure();
        }
    }

    /// Send a configure for the latest window properties.
    pub fn reconfigure(&mut self) {
        let result = self.surface.with_pending_state(|state| {
            state.size = Some(self.rectangle.size);
        });

        if result.is_ok() {
            self.surface.send_configure();
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
