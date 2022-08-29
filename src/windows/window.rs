//! Wayland client window.

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use std::sync::Mutex;

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::{self, BufferType, ImportAll};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Physical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::shell::wlr_layer::{
    Anchor, ExclusiveZone, KeyboardInteractivity, LayerSurfaceCachedState,
};
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface, XdgPopupSurfaceRoleAttributes};

use crate::drawing::{SurfaceBuffer, Texture};
use crate::output::{ExclusiveSpace, Output};
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};
use crate::windows::Transaction;

/// Wayland client window state.
#[derive(Debug)]
pub struct Window<S = ToplevelSurface> {
    /// Initial size configure status.
    pub initial_configure_sent: bool,

    /// Buffers pending to be imported.
    pub buffers_pending: bool,

    /// Last configure size acked by the client.
    pub acked_size: Size<i32, Logical>,

    /// Whether the window should be excluded for keyboard focus.
    pub deny_focus: bool,

    /// Attached surface.
    pub surface: S,

    /// Desired window dimensions.
    rectangle: Rectangle<i32, Logical>,

    /// Texture cache, storing last window state.
    texture_cache: TextureCache,

    /// Window is currently visible on the output.
    visible: bool,

    /// Transaction for atomic upgrades.
    transaction: Option<WindowTransaction>,

    /// Popup windows.
    popups: Vec<Window<PopupSurface>>,

    /// Pending window damage.
    damage: Option<Rectangle<i32, Physical>>,
}

impl<S: Surface> Window<S> {
    /// Create a new Toplevel window.
    pub fn new(surface: S) -> Self {
        Window {
            surface,
            initial_configure_sent: Default::default(),
            buffers_pending: Default::default(),
            texture_cache: Default::default(),
            transaction: Default::default(),
            deny_focus: Default::default(),
            acked_size: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
            popups: Default::default(),
            damage: Default::default(),
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

        // Request new frames from all popups.
        for window in &mut self.popups {
            window.request_frame(runtime);
        }
    }

    /// Geometry of the window's visible bounds.
    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        self.surface.geometry()
    }

    /// Check window liveliness.
    pub fn alive(&self) -> bool {
        self.surface.alive()
    }

    /// Check if this window contains a specific point.
    pub fn contains(&self, point: Point<f64, Logical>) -> bool {
        self.bounds().to_f64().contains(point)
    }

    /// Render this window's buffers.
    ///
    /// If no location is specified, the textures cached location will be used.
    pub fn draw<'a>(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        scale: f64,
        bounds: impl Into<Option<Rectangle<i32, Logical>>>,
        damage: impl Into<Option<&'a [Rectangle<i32, Physical>]>>,
    ) {
        // Skip updating windows during transactions.
        if self.transaction.is_none() && self.buffers_pending {
            self.import_buffers(renderer);
        }

        let bounds = bounds.into().unwrap_or_else(|| self.bounds());
        let physical_bounds = bounds.to_physical(output.scale());

        // Treat no damage information as full damage.
        let full_damage = [physical_bounds];
        let damage = damage.into().unwrap_or(&full_damage);

        // Calculate damage overlapping this window.
        let mut window_damage = None;
        for damage in damage.iter().filter_map(|damage| damage.intersection(physical_bounds)) {
            window_damage = Some(match window_damage {
                Some(window_damage) => damage.merge(window_damage),
                None => damage,
            });
        }

        // Skip rendering without damage.
        let window_damage = match window_damage {
            Some(window_damage) => window_damage,
            None => return,
        };

        // Clear window damage.
        self.damage = None;

        for texture in &mut self.texture_cache.textures {
            texture.draw_at(frame, output, bounds, scale, window_damage);
        }

        // Draw popup tree.
        for popup in &mut self.popups {
            let loc = bounds.loc + popup.rectangle.loc;
            let popup_bounds = Rectangle::from_loc_and_size(loc, (i32::MAX, i32::MAX));
            popup.draw(renderer, frame, output, scale, popup_bounds, damage);
        }
    }

    /// Default window location and size.
    fn bounds(&self) -> Rectangle<i32, Logical> {
        // Center window inside its space.
        let mut bounds = self.rectangle;
        bounds.loc.x += ((bounds.size.w - self.texture_cache.size.w) / 2).max(0);
        bounds.loc.y += ((bounds.size.h - self.texture_cache.size.h) / 2).max(0);
        bounds
    }

    /// Import the buffers of all surfaces into the renderer.
    fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        let geometry = self.geometry();
        self.texture_cache.reset(geometry.size);
        self.buffers_pending = false;

        compositor::with_surface_tree_upward(
            self.surface.surface(),
            Point::from((0, 0)) - geometry.loc,
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

                // Skip surface if buffer was already imported.
                if let Some(texture) = &data.texture {
                    self.texture_cache.push(texture.clone());
                    return TraversalAction::DoChildren(location);
                }

                // Retrieve and clear damage.
                let damage = data.damage.buffer();

                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                // Import and cache the buffer.
                let action = match renderer.import_buffer(buffer, Some(surface_data), damage) {
                    Some(Ok(texture)) => {
                        // Release SHM buffers after import.
                        if let Some(BufferType::Shm) = renderer::buffer_type(buffer) {
                            data.buffer = None;
                        }

                        // Update and cache the texture.
                        let texture = Texture::from_surface(Rc::new(texture), location, &data);
                        self.texture_cache.push(texture.clone());
                        data.texture = Some(texture);

                        TraversalAction::DoChildren(location)
                    },
                    _ => {
                        eprintln!("unable to import buffer");
                        data.buffer = None;

                        TraversalAction::SkipChildren
                    },
                };

                // Ensure damage is cleared after import.
                data.damage.clear_buffer();

                action
            },
            |_, _, _| (),
            |_, _, _| true,
        );
    }

    /// Send a configure for the latest window properties.
    fn reconfigure(&mut self) {
        let size = match &self.transaction {
            Some(transaction) => transaction.rectangle.size,
            None => self.rectangle.size,
        };

        self.surface.reconfigure(size);
    }

    /// Change the window dimensions.
    pub fn set_dimensions(
        &mut self,
        transaction: &Transaction,
        rectangle: Rectangle<i32, Logical>,
    ) {
        // Prevent redundant configure events.
        let transaction = self.start_transaction(transaction);
        let old_ractangle = mem::replace(&mut transaction.rectangle, rectangle);
        if transaction.rectangle != old_ractangle && self.initial_configure_sent {
            self.reconfigure();
        }
    }

    /// Send output enter event to this window's surfaces.
    pub fn enter(&mut self, output: &Output) {
        self.with_surfaces(|surface, _| output.enter(surface));
        self.visible = true;
    }

    /// Send output leave event to this window's surfaces.
    pub fn leave(&mut self, transaction: &Transaction, output: &Output) {
        self.with_surfaces(|surface, _| output.leave(surface));
        self.visible = false;

        // Resize to fullscreen for app overview.
        let rectangle = Rectangle::from_loc_and_size((0, 0), output.available().size);
        self.set_dimensions(transaction, rectangle);
    }

    /// Execute a function for all surfaces of this window.
    fn with_surfaces<F: FnMut(&WlSurface, &SurfaceData)>(&self, mut fun: F) {
        compositor::with_surface_tree_upward(
            self.surface.surface(),
            (),
            |_, _, _| TraversalAction::DoChildren(()),
            |surface, surface_data, _| fun(surface, surface_data),
            |_, _, _| true,
        );
    }

    /// Create a new transaction, or access the active one.
    ///
    /// Takes in a transaction as parameter to ensure the window will not get
    /// stuck in the frozen state indefinitely.
    fn start_transaction(&mut self, _transaction: &Transaction) -> &mut WindowTransaction {
        self.transaction.get_or_insert(WindowTransaction::new(self))
    }

    /// Apply all staged changes if there is a transaction.
    pub fn apply_transaction(&mut self) {
        let transaction = match self.transaction.take() {
            Some(transaction) => transaction,
            None => return,
        };

        self.rectangle = transaction.rectangle;
    }

    /// Check if the transaction is ready for application.
    pub fn transaction_done(&self) -> bool {
        self.transaction.as_ref().map_or(true, |t| t.rectangle.size == self.acked_size)
    }

    /// Handle common surface commit logic for surfaces of any kind.
    pub fn surface_commit_common(&mut self, surface: &WlSurface, output: &Output) {
        // Cancel transactions on the commit after the configure was acked.
        self.acked_size = self.surface.acked_size();

        // Handle surface buffer changes.
        let mut new_surfaces = Vec::new();
        compositor::with_surface_tree_upward(
            surface,
            self.bounds().loc,
            |surface, data, location| {
                // Compute absolute surface location.
                let mut location = *location;
                if data.role == Some("subsurface") {
                    let current = data.cached_state.current::<SubsurfaceCachedState>();
                    location += current.location;
                }

                // Retrieve new buffer updates.
                let mut attributes = data.cached_state.current::<SurfaceAttributes>();
                let assignment = match attributes.buffer.take() {
                    Some(assignment) => assignment,
                    None => return TraversalAction::DoChildren(location),
                };

                // Store pending buffer updates.
                let is_new = data.data_map.insert_if_missing(|| RefCell::new(SurfaceBuffer::new()));
                let mut buffer =
                    data.data_map.get::<RefCell<SurfaceBuffer>>().unwrap().borrow_mut();
                buffer.update_buffer(&mut attributes, assignment, output.scale());
                self.buffers_pending = true;

                // Collect surfaces created in this commit.
                if is_new {
                    new_surfaces.push(surface.clone());
                }

                // Update window damage.
                for mut damage in buffer.damage.drain_physical() {
                    damage.loc += location.to_physical(output.scale());
                    self.damage = Some(match self.damage {
                        Some(window_damage) => window_damage.merge(damage),
                        None => damage,
                    });
                }

                TraversalAction::DoChildren(location)
            },
            |_, _, _| (),
            |_, _, _| true,
        );

        // Ensure buffer is cleaned up when surface dies.
        //
        // NOTE: This is necessary since the `SurfaceData` is never dropped by Smithay,
        // even if the window is dead and no more references are held to it in
        // Catacomb.
        for surface in new_surfaces {
            compositor::add_destruction_hook(&surface, |data| {
                if let Some(data) = data.data_map.get::<RefCell<SurfaceBuffer>>() {
                    *data.borrow_mut() = Default::default();
                }
            });
        }

        // Update damage if window moved within its bounds due to centering.
        let new_size = self.geometry().size;
        let old_size = self.texture_cache.size;
        if let Some(damage) = self.damage.as_mut().filter(|_| new_size != old_size) {
            let mut moved_damage = *damage;
            moved_damage.loc += (old_size - new_size).to_physical(output.scale());
            *damage = damage.merge(moved_damage);
        }

        // Send initial configure after the first commit.
        if !self.initial_configure_sent {
            self.initial_configure_sent = true;
            self.reconfigure();
        }

        // Advertise current output to visible surfaces.
        if self.visible {
            output.enter(surface);
        }
    }

    /// Find subsurface at the specified location.
    pub fn surface_at(&self, position: Point<f64, Logical>) -> Option<OffsetSurface> {
        // Check popups top to bottom first.
        let relative = position - self.rectangle.loc.to_f64();
        let popup = self.popups.iter().find_map(|popup| popup.surface_at(relative));
        if let Some(popup_surface) = popup {
            return Some(popup_surface);
        }

        let result = RefCell::new(None);
        compositor::with_surface_tree_upward(
            self.surface.surface(),
            self.bounds().loc,
            |wl_surface, surface_data, location| {
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    let current = surface_data.cached_state.current::<SubsurfaceCachedState>();
                    location += current.location;
                }

                // Calculate surface's bounding box.
                let size = surface_data
                    .data_map
                    .get::<RefCell<SurfaceBuffer>>()
                    .map_or_else(Size::default, |data| data.borrow().size);
                let surface_rect = Rectangle::from_loc_and_size(location.to_f64(), size.to_f64());

                // Check if the position is within the surface bounds.
                if surface_rect.contains(position) {
                    let surface = OffsetSurface::new(wl_surface.clone(), location);
                    *result.borrow_mut() = Some(surface);
                    TraversalAction::SkipChildren
                } else {
                    TraversalAction::DoChildren(location)
                }
            },
            |_, _, _| {},
            |_, _, _| result.borrow().is_none(),
        );
        result.into_inner()
    }

    /// Add popup at the correct position in the popup tree.
    ///
    /// This function will return the popup when no matching parent surface
    /// could be found in the popup tree.
    pub fn add_popup(
        &mut self,
        mut popup: Window<PopupSurface>,
        parent: &WlSurface,
    ) -> Option<Window<PopupSurface>> {
        if self.surface.surface() == parent {
            self.popups.push(popup);
            return None;
        }

        for window in &mut self.popups {
            popup = match window.add_popup(popup, parent) {
                Some(popup) => popup,
                None => return None,
            };
        }

        Some(popup)
    }

    /// Apply surface commits for popups.
    pub fn popup_surface_commit(
        &mut self,
        root_surface: &WlSurface,
        surface: &WlSurface,
        output: &Output,
    ) {
        for window in &mut self.popups {
            if window.surface.surface() == root_surface {
                window.surface_commit_common(surface, output);
                window.rectangle.loc = window.position();
                return;
            }

            window.popup_surface_commit(root_surface, surface, output);
        }
    }

    /// Refresh popup windows.
    pub fn refresh_popups(&mut self) {
        for i in (0..self.popups.len()).rev() {
            self.popups[i].refresh_popups();

            if !self.popups[i].alive() {
                self.popups.swap_remove(i);
            }
        }
    }

    /// Check if this window requires a redraw.
    pub fn damaged(&self) -> bool {
        self.transaction.is_none() && self.damage.is_some()
    }

    /// Get pending window damage.
    pub fn damage(&self) -> Option<Rectangle<i32, Physical>> {
        self.damage.filter(|_| self.transaction.is_none())
    }

    /// Get primary window surface.
    pub fn surface(&self) -> &WlSurface {
        self.surface.surface()
    }
}

impl Window<PopupSurface> {
    /// Get the parent of this popup.
    pub fn parent(&self) -> Option<WlSurface> {
        compositor::with_states(self.surface.surface(), |states| {
            let attributes = states.data_map.get::<Mutex<XdgPopupSurfaceRoleAttributes>>()?;
            attributes.lock().ok()?.parent.clone()
        })
    }

    /// Get popup window offset from parent.
    fn position(&self) -> Point<i32, Logical> {
        self.surface.with_pending_state(|state| state.positioner.get_geometry().loc)
    }
}

impl Window<CatacombLayerSurface> {
    /// Handle a surface commit for layer shell windows.
    pub fn surface_commit(
        &mut self,
        surface: &WlSurface,
        output: &mut Output,
        transaction: &Transaction,
    ) {
        self.update_layer_state(output);
        self.update_dimensions(output, transaction);
        self.surface_commit_common(surface, output);
    }

    /// Recompute the window's size and location.
    pub fn update_dimensions(&mut self, output: &mut Output, transaction: &Transaction) {
        let state = compositor::with_states(self.surface.surface(), |states| {
            *states.cached_state.current::<LayerSurfaceCachedState>()
        });
        let output_size = output.size();
        let mut size = state.size;

        let exclusive = match state.exclusive_zone {
            ExclusiveZone::Neutral => output.exclusive,
            _ => ExclusiveSpace::default(),
        };

        // Window size.
        if size.w == 0 && state.anchor.contains(Anchor::LEFT | Anchor::RIGHT) {
            size.w = output_size.w - exclusive.left - exclusive.right;
            if state.anchor.contains(Anchor::RIGHT) {
                size.w -= state.margin.right;
            }
        }
        if size.h == 0 && state.anchor.contains(Anchor::TOP | Anchor::BOTTOM) {
            size.h = output_size.h - exclusive.top - exclusive.bottom;
            if state.anchor.contains(Anchor::BOTTOM) {
                size.h -= state.margin.bottom;
            }
        }

        // Window location.
        let x = if state.anchor.contains(Anchor::LEFT) {
            state.margin.left + exclusive.left
        } else if state.anchor.contains(Anchor::RIGHT) {
            output_size.w - size.w - state.margin.right - exclusive.right
        } else {
            (output_size.w - size.w) / 2
        };
        let y = if state.anchor.contains(Anchor::TOP) {
            state.margin.top + exclusive.top
        } else if state.anchor.contains(Anchor::BOTTOM) {
            output_size.h - size.h - state.margin.bottom - exclusive.bottom
        } else {
            (output_size.h - size.h) / 2
        };

        let dimensions = Rectangle::from_loc_and_size((x, y), size);
        self.set_dimensions(transaction, dimensions);
    }

    /// Update layer-specific shell properties.
    fn update_layer_state(&mut self, output: &mut Output) {
        let state = compositor::with_states(self.surface.surface(), |states| {
            *states.cached_state.current::<LayerSurfaceCachedState>()
        });

        // Update keyboard interactivity.
        self.deny_focus = state.keyboard_interactivity == KeyboardInteractivity::None;

        // Update exclusive zones.
        let old_exclusive = mem::replace(&mut self.surface.exclusive_zone, state.exclusive_zone);
        let old_anchor = mem::replace(&mut self.surface.anchor, state.anchor);
        output.exclusive.reset(old_anchor, old_exclusive);
        output.exclusive.update(state.anchor, state.exclusive_zone);
    }
}

/// Atomic changes to [`Window`].
#[derive(Debug)]
struct WindowTransaction {
    rectangle: Rectangle<i32, Logical>,
}

impl WindowTransaction {
    fn new<S>(current_state: &Window<S>) -> Self {
        Self { rectangle: current_state.rectangle }
    }
}

/// Cached window textures.
#[derive(Default, Debug)]
struct TextureCache {
    /// Size of all textures combined.
    size: Size<i32, Logical>,
    textures: Vec<Texture>,
}

impl TextureCache {
    /// Reset the texture cache.
    fn reset(&mut self, size: Size<i32, Logical>) {
        self.textures.clear();
        self.size = size;
    }

    /// Add a new texture.
    fn push(&mut self, texture: Texture) {
        self.textures.push(texture);
    }
}
