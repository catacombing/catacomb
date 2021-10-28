//! Wayland client.

use std::cell::RefCell;
use std::mem;
use std::rc::{Rc, Weak};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
use smithay::wayland::shell::xdg::ToplevelSurface;

use crate::output::Output;
use crate::shell::SurfaceBuffer;

/// Container tracking all known clients.
#[derive(Default)]
pub struct Windows {
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    windows: Vec<Rc<RefCell<Window>>>,
}

impl Windows {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        if self.primary.strong_count() > 0 && self.secondary.strong_count() == 0 {
            // Spawn as secondary if there's only one primary window.
            self.secondary = Rc::downgrade(&window);
        } else {
            // Spawn as maximized primary if empty or secondary already present.
            self.primary = Rc::downgrade(&window);
            self.secondary = Weak::new();
        }
        self.windows.push(window);
    }

    /// Execute a function for all visible windows.
    pub fn with_visible<F: FnMut(&mut Window)>(&self, mut fun: F) {
        for window in self.primary.upgrade().iter_mut().chain(&mut self.secondary.upgrade()) {
            fun(&mut window.borrow_mut());
        }
    }

    /// Refresh the client list.
    ///
    /// This function will remove all dead windows and update the remaining ones appropriately.
    pub fn refresh(&mut self, output_size: Size<i32, Logical>) {
        // Remove dead windows.
        self.windows.retain(|window| window.borrow().surface.alive());

        // Replace dead primary windows with the secondary.
        if self.primary.strong_count() == 0 && self.secondary.strong_count() > 0 {
            mem::swap(&mut self.primary, &mut self.secondary);
        }

        self.update_dimensions(output_size);
    }

    /// Update size and location of visible windows.
    pub fn update_dimensions(&mut self, mut output_size: Size<i32, Logical>) {
        if let Some(secondary) = self.secondary.upgrade() {
            let mut secondary = secondary.borrow_mut();
            let height = output_size.h as f32 / 2.;
            secondary.rectangle.loc = Point::from((0, height.ceil() as i32));
            secondary.resize(Size::from((output_size.w, height.floor() as i32)));

            if secondary.has_buffer() {
                output_size = Size::from((output_size.w, height.ceil() as i32));
            }
        }

        if let Some(primary) = self.primary.upgrade() {
            let mut primary = primary.borrow_mut();
            primary.rectangle.loc = Point::from((0, 0));
            primary.resize(output_size);
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
    rectangle: Rectangle<i32, Logical>,
    surface: ToplevelSurface,

    /// Texture cache, for rendering dead windows.
    textures: Vec<Texture>,
}

impl Window {
    pub fn new(surface: ToplevelSurface) -> Self {
        Window { rectangle: Rectangle::default(), textures: Vec::new(), surface }
    }

    /// Check if this window has a drawable buffer attached.
    pub fn has_buffer(&self) -> bool {
        let wl_surface = match self.surface.get_surface() {
            Some(wl_surface) => wl_surface,
            None => return false,
        };

        let mut has_buffer = false;
        compositor::with_surface_tree_upward(
            &wl_surface,
            (),
            |_, surface_data, _| {
                let buffer = surface_data.data_map.get::<RefCell<SurfaceBuffer>>();
                has_buffer |= buffer.map_or(false, |data| data.borrow().buffer.is_some());

                if has_buffer {
                    return TraversalAction::SkipChildren;
                } else {
                    return TraversalAction::DoChildren(());
                }
            },
            |_, _, _| (),
            |_, _, _| true,
        );
        has_buffer
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
    pub fn resize(&mut self, size: Size<i32, Logical>) {
        // Prevent redundant configure events.
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
        self.import_buffers(renderer);

        // TODO: Assure rendering from cached doesn't impede perf.
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
