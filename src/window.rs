//! Wayland client.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::mem;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::reexports::wayland_protocols::unstable::xdg_decoration;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::shell::xdg::{SurfaceCachedState, ToplevelSurface};
use wayland_protocols::xdg_shell::server::xdg_toplevel::State;
use xdg_decoration::v1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;

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
    pub fn add(&mut self, surface: ToplevelSurface, output: &Output) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        if self.primary.strong_count() > 0 && self.secondary.strong_count() == 0 {
            self.set_secondary(output, &window);
        } else {
            self.set_primary(output, &window);
            self.set_secondary(output, None);
        }
        self.windows.push(window);
    }

    /// Find the window responsible for a specific surface.
    pub fn find(&mut self, wl_surface: &WlSurface) -> Option<RefMut<Window>> {
        // Get root surface.
        let mut wl_surface = Cow::Borrowed(wl_surface);
        while let Some(surface) = compositor::get_parent(&wl_surface) {
            wl_surface = Cow::Owned(surface);
        }

        self.windows.iter_mut().map(|window| window.borrow_mut()).find(|window| {
            window.surface.get_surface().map_or(false, |surface| surface.eq(&wl_surface))
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
    pub fn refresh(&mut self, output: &Output) {
        // Remove dead visible windows.
        if self.primary.upgrade().map_or(false, |primary| !primary.borrow().surface.alive()) {
            self.set_primary(output, None);
        }
        if self.secondary.upgrade().map_or(false, |secondary| !secondary.borrow().surface.alive()) {
            self.set_secondary(output, None);
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
            let finished =
                self.windows.iter().map(|window| window.borrow()).all(|window| {
                    !window.frozen || window.geometry().size == window.rectangle.size
                });

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

    /// Update window dimensions.
    pub fn update_dimensions(&mut self, output: &Output) {
        self.transaction_start.get_or_insert_with(Instant::now);

        if let Some(mut primary) = self.primary.upgrade().as_ref().map(|p| p.borrow_mut()) {
            let secondary_visible = self.secondary.strong_count() > 0;
            let rectangle = output.primary_rectangle(primary.geometry(), secondary_visible);
            primary.rectangle.loc = rectangle.loc;
            primary.resize(rectangle.size);
            primary.frozen = true;
        }

        if let Some(mut secondary) = self.secondary.upgrade().as_ref().map(|s| s.borrow_mut()) {
            let rectangle = output.secondary_rectangle(secondary.geometry());
            secondary.rectangle.loc = rectangle.loc;
            secondary.resize(rectangle.size);
            secondary.frozen = true;
        }
    }

    /// Change the primary window.
    fn set_primary<'a>(
        &mut self,
        output: &Output,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
    ) {
        let window = window.into();

        // Update output's visible windows.
        if let Some(primary) = self.primary.upgrade() {
            primary.borrow_mut().leave(output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Copy last buffer state to new windows.
        let window = window.cloned().or_else(|| mem::take(&mut self.secondary).upgrade());
        if let Some((window, primary)) = window.as_ref().zip(self.primary.upgrade()) {
            window.borrow_mut().texture_cache.append(&mut primary.borrow_mut().texture_cache);
        }

        self.primary = window.map(|window| Rc::downgrade(&window)).unwrap_or_default();
        self.update_dimensions(output);
    }

    /// Change the secondary window.
    fn set_secondary<'a>(
        &mut self,
        output: &Output,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
    ) {
        let window = window.into();

        // Update output's visible windows.
        if let Some(secondary) = self.secondary.upgrade() {
            secondary.borrow_mut().leave(output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Copy last buffer state to new windows.
        if let (Some(window), _, Some(secondary)) | (None, Some(window), Some(secondary)) =
            (window, &self.primary.upgrade(), self.secondary.upgrade())
        {
            window.borrow_mut().texture_cache.append(&mut secondary.borrow_mut().texture_cache);
        }

        // Replace window and start transaction for updates.
        self.secondary = window.map(Rc::downgrade).unwrap_or_default();
        self.update_dimensions(output);
    }
}

/// Cached window textures.
#[derive(Default, Debug)]
struct TextureCache {
    /// Base window position.
    ///
    /// Last window origin which the surfaces' location offsets are based on.
    location: Point<i32, Logical>,
    textures: Vec<Texture>,
}

impl TextureCache {
    /// Reset the texture cache.
    fn reset(&mut self, location: Point<i32, Logical>) {
        self.location = location;
        self.textures.clear();
    }

    /// Add a new texture.
    fn push(&mut self, texture: Texture) {
        self.textures.push(texture);
    }

    /// Combine two texture caches.
    fn append(&mut self, other: &mut Self) {
        // Change the surfaces' location offset to the new base window position.
        let delta = other.location - self.location;
        for texture in &mut other.textures {
            texture.location += delta;
        }

        self.textures.append(&mut other.textures);
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
    /// Initial size configure status.
    pub initial_configure_sent: bool,

    /// Buffers pending to be imported.
    pub buffers_pending: bool,

    /// Desired window dimensions.
    rectangle: Rectangle<i32, Logical>,

    /// Attached surface.
    surface: ToplevelSurface,

    /// Texture cache, storing last window state.
    texture_cache: TextureCache,

    /// Window is currently visible on the output.
    visible: bool,

    /// Freeze window updates to allow atomic upgrades.
    frozen: bool,
}

impl Window {
    pub fn new(surface: ToplevelSurface) -> Self {
        Window {
            surface,
            initial_configure_sent: Default::default(),
            buffers_pending: Default::default(),
            texture_cache: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
            frozen: Default::default(),
        }
    }

    /// Send a frame request to the window.
    pub fn request_frame(&mut self, runtime: u32) {
        self.with_surfaces(|_, surface_data| {
            let mut attributes = surface_data.cached_state.current::<SurfaceAttributes>();
            for callback in attributes.frame_callbacks.drain(..) {
                callback.done(runtime);
            }
        });
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

            // Mark window as tiled, using maximized fallback if it is unsupported.
            if self.surface.version() >= 2 {
                state.states.set(State::TiledBottom);
                state.states.set(State::TiledRight);
                state.states.set(State::TiledLeft);
                state.states.set(State::TiledTop);
            } else {
                state.states.set(State::Maximized);
            }

            // Always use server-side decorations.
            state.decoration_mode = Some(DecorationMode::ServerSide);
        });

        if result.is_ok() {
            self.surface.send_configure();
        }
    }

    /// Send output enter event to this window's surfaces.
    pub fn enter(&mut self, output: &Output) {
        self.with_surfaces(|surface, _| output.enter(surface));
        self.visible = true;
    }

    /// Send output leave event to this window's surfaces.
    pub fn leave(&mut self, output: &Output) {
        self.with_surfaces(|surface, _| output.leave(surface));
        self.visible = false;
    }

    /// Render this window's buffers.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output: &Output) {
        // Skip updating windows during transactions.
        if !self.frozen && self.buffers_pending {
            self.import_buffers(renderer);
        }

        for Texture { texture, location, scale } in &self.texture_cache.textures {
            let location = self.texture_cache.location + *location;
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

    /// Check if window is visible on the output.
    pub fn visible(&self) -> bool {
        self.visible
    }

    /// Geometry of the window's visible bounds.
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.surface
            .get_surface()
            .and_then(|surface| {
                compositor::with_states(&surface, |states| {
                    states.cached_state.current::<SurfaceCachedState>().geometry
                })
                .ok()
                .flatten()
            })
            .unwrap_or_default()
    }

    /// Import the buffers of all surfaces into the renderer.
    fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        // Ensure there is a drawable surface present.
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        self.texture_cache.reset(self.rectangle.loc);
        self.buffers_pending = false;

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
                    let subsurface = surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += subsurface.location;
                }

                let attributes = surface_data.cached_state.current::<SurfaceAttributes>();
                let scale = attributes.buffer_scale;

                // Skip surface if buffer was already imported.
                if let Some(texture) = &data.texture {
                    self.texture_cache.push(Texture::new(texture.clone(), location, scale));
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
                        self.texture_cache.push(Texture::new(texture, location, scale));

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

    /// Execute a function for all surfaces of this window.
    fn with_surfaces<F: FnMut(&WlSurface, &SurfaceData)>(&mut self, mut fun: F) {
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        compositor::with_surface_tree_upward(
            wl_surface,
            (),
            |_, _, _| TraversalAction::DoChildren(()),
            |surface, surface_data, _| fun(surface, surface_data),
            |_, _, _| true,
        );
    }
}
