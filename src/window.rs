//! Wayland client.

use std::cell::RefCell;
use std::slice::{Iter, IterMut};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::utils::{Logical, Point, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
use smithay::wayland::shell::xdg::ToplevelSurface;

use crate::output::Output;
use crate::shell::SurfaceBuffer;

/// Container tracking all known clients.
#[derive(Default)]
pub struct Windows {
    windows: Vec<Window>,
    primary: Option<usize>,
    secondary: Option<usize>,
}

impl Windows {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new window.
    pub fn push(&mut self, window: Window) {
        match window.location {
            WindowLocation::Primary => {
                if let Some(index) = self.primary {
                    self.windows[index].location = WindowLocation::Hidden;
                }
                self.primary = Some(self.windows.len());
            },
            WindowLocation::Secondary => {
                if let Some(index) = self.secondary {
                    self.windows[index].location = WindowLocation::Hidden;
                }
                self.secondary = Some(self.windows.len());
            },
            WindowLocation::Hidden => (),
        }
        self.windows.push(window);
    }

    /// Returns an immutable iterator over all windows.
    pub fn iter(&self) -> Iter<'_, Window> {
        self.windows.iter()
    }

    /// Returns a mutable iterator over all windows.
    pub fn iter_mut(&mut self) -> IterMut<'_, Window> {
        self.windows.iter_mut()
    }

    /// Refresh the client list.
    ///
    /// This function will remove all dead windows and update the remaining ones appropriately.
    pub fn refresh(&mut self, output_size: Size<i32, Logical>) {
        // Handle removal of primary windows.
        if self.primary.map_or(false, |index| !self.windows[index].surface.alive()) {
            self.primary = self.secondary;
            if let Some(window) = self.secondary.take().map(|index| &mut self.windows[index]) {
                window.location = WindowLocation::Primary;
                window.resize(output_size);
            }
        }

        // Handle removal of secondary windows.
        if self.secondary.map_or(false, |index| !self.windows[index].surface.alive()) {
            if let Some(primary_index) = self.primary {
                self.windows[primary_index].resize(output_size);
            }
            self.secondary = None;
        }

        // Remove all dead windows.
        let mut i = 0;
        self.windows.retain(|window| {
            let alive = window.surface.alive();

            // Update indices if an element was removed.
            if !alive {
                let iter = self.primary.iter_mut().chain(&mut self.secondary);
                for index in iter.filter(|index| i < **index) {
                    *index -= 1;
                }
            }
            i += 1;

            alive
        });
    }
}

/// Wayland client window state.
pub struct Window {
    pub surface: ToplevelSurface,
    pub location: WindowLocation,
}

impl Window {
    pub fn new(surface: ToplevelSurface, location: WindowLocation) -> Self {
        Window { surface, location }
    }

    /// Send a frame request to the window.
    pub fn request_frame(&self, runtime: u32) {
        // Ensure there is a drawable surface present.
        let wl_surface = match (self.location, self.surface.get_surface()) {
            (WindowLocation::Hidden, _) | (_, None) => return,
            (_, Some(surface)) => surface,
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
    pub fn draw(&self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output: &Output) {
        // Ensure there is a drawable surface present.
        let wl_surface = match (self.location, self.surface.get_surface()) {
            (WindowLocation::Hidden, _) | (_, None) => return,
            (_, Some(surface)) => surface,
        };

        // Determine window origin point.
        let mut location = Point::from((0, 0));
        if self.location == WindowLocation::Secondary {
            location.y = output.size().h / 2;
        }

        compositor::with_surface_tree_upward(
            wl_surface,
            location,
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
                    let subsurface = surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += subsurface.location;
                }

                let attributes = surface_data.cached_state.current::<SurfaceAttributes>();

                let _ = frame.render_texture_at(
                    texture,
                    location.to_f64().to_physical(output.scale).to_i32_round(),
                    attributes.buffer_scale,
                    output.scale,
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

/// Window positioning.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum WindowLocation {
    /// Primary window.
    ///
    /// This window will be maximized if no [`WindowLocation::Secondary`] window is visible.
    /// Otherwise it will be to the top of the [`WindowLocation::Secondary`] window in portrait
    /// mode and to the left of it in landscape mode.
    Primary,

    /// Secondary window
    ///
    /// This window is tiled below the [`WindowLocation::Primary`] window in portrait mode and to
    /// the right of it in landscape mode.
    Secondary,

    /// Window which is currently invisible.
    Hidden,
}
