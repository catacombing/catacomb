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
    self, RectangleKind, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::shell::wlr_layer::{
    Anchor, ExclusiveZone, KeyboardInteractivity, Layer, LayerSurfaceCachedState,
};
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface, XdgPopupSurfaceRoleAttributes};

use crate::drawing::{CatacombSurfaceData, Texture};
use crate::geometry::{SubtractRectFast, Vector};
use crate::output::{Canvas, ExclusiveSpace, Output};
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};
use crate::windows::{self, OpaqueRegions};

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
    damage: Option<Rectangle<i32, Logical>>,

    /// Opaque window region.
    opaque_region: Vec<Rectangle<i32, Logical>>,

    /// Window liveliness override.
    dead: bool,
}

impl<S: Surface> Window<S> {
    /// Create a new Toplevel window.
    pub fn new(surface: S) -> Self {
        Window {
            surface,
            initial_configure_sent: Default::default(),
            buffers_pending: Default::default(),
            opaque_region: Default::default(),
            texture_cache: Default::default(),
            transaction: Default::default(),
            deny_focus: Default::default(),
            acked_size: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
            popups: Default::default(),
            damage: Default::default(),
            dead: Default::default(),
        }
    }

    /// Send a frame request to the window.
    pub fn request_frame(&self, runtime: u32) {
        self.with_surfaces(|_, surface_data| {
            let mut attributes = surface_data.cached_state.current::<SurfaceAttributes>();
            for callback in attributes.frame_callbacks.drain(..) {
                callback.done(runtime);
            }
        });

        // Request new frames from all popups.
        for window in &self.popups {
            window.request_frame(runtime);
        }
    }

    /// Geometry of the window's visible bounds.
    pub fn geometry(&self) -> Rectangle<i32, Logical> {
        self.surface.geometry()
    }

    /// Check window liveliness.
    pub fn alive(&self) -> bool {
        !self.dead && self.surface.alive()
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
        frame: &mut Gles2Frame,
        canvas: &Canvas,
        scale: f64,
        bounds: impl Into<Option<Rectangle<i32, Logical>>>,
        damage: impl Into<Option<&'a [Rectangle<i32, Physical>]>>,
        opaque_regions: impl Into<Option<&'a mut OpaqueRegions>>,
    ) {
        let bounds = bounds.into().unwrap_or_else(|| self.bounds());
        let physical_bounds = bounds.to_physical(canvas.scale());

        // Treat no damage information as full damage.
        let full_damage = [physical_bounds];
        let damage = damage.into().unwrap_or(&full_damage);

        // Calculate damage overlapping this window.
        let mut window_damage = damage
            .iter()
            .filter_map(|damage| damage.intersection(physical_bounds))
            .reduce(Rectangle::merge);

        // Handle opaque regions.
        let mut opaque_regions = opaque_regions.into();
        if let Some(opaque_regions) = opaque_regions.as_mut() {
            // Remove this window's region from opaque regions.
            opaque_regions.popn(self.opaque_region.len());

            // Filter out occluded damage.
            if let Some(damage) = window_damage {
                let unoccluded_damage = opaque_regions.filter_damage(&[damage]);
                window_damage = unoccluded_damage.iter().copied().reduce(Rectangle::merge);
            }
        }

        // Clear window damage.
        //
        // Since window damage only matters for the current frame, we clear it even when
        // nothing needs to be drawn since all the required updates have been completed
        // at that point.
        self.damage = None;

        // Skip rendering without damage.
        let window_damage = match window_damage {
            Some(window_damage) if !window_damage.is_empty() => window_damage,
            _ => return,
        };

        for texture in &mut self.texture_cache.textures {
            texture.draw_at(frame, canvas, bounds, scale, window_damage);
        }

        // Draw popup tree.
        for popup in &mut self.popups {
            let loc = bounds.loc + popup.rectangle.loc;
            let popup_bounds = Rectangle::from_loc_and_size(loc, (i32::MAX, i32::MAX));
            popup.draw(frame, canvas, scale, popup_bounds, damage, opaque_regions.as_deref_mut());
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
    pub fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        // Do not import buffers during a transaction.
        if self.transaction.is_some() {
            return;
        }

        // Import buffers for all popup windows.
        for popup in &mut self.popups {
            popup.import_buffers(renderer);
        }

        // Short-circuit if we know no new buffer is waiting for import.
        if !self.buffers_pending {
            return;
        }

        let geometry = self.geometry();
        let old_size = self.texture_cache.size;
        self.texture_cache.reset(geometry.size);
        self.buffers_pending = false;

        compositor::with_surface_tree_upward(
            self.surface.surface(),
            Point::from((0, 0)) - geometry.loc,
            |_, surface_data, location| {
                let data = match surface_data.data_map.get::<RefCell<CatacombSurfaceData>>() {
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

                // Update window's opaque region.
                self.opaque_region.clear();
                for (kind, rect) in &data.opaque_region {
                    let mut rect = *rect;
                    rect.loc += location;

                    match kind {
                        RectangleKind::Add => self.opaque_region.push(rect),
                        RectangleKind::Subtract => self.opaque_region.subtract_rect(rect),
                    }
                }

                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                // Retrieve buffer damage.
                let damage = data.damage.buffer();

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

                // Update window damage.
                let new_damage = data.damage.logical().iter().copied().flat_map(|mut damage| {
                    damage.loc += location;
                    damage.intersection(geometry)
                });
                self.damage = new_damage.chain(self.damage).reduce(Rectangle::merge);

                // Ensure damage is cleared after successful import.
                data.damage.clear();

                action
            },
            |_, _, _| (),
            |_, _, _| true,
        );

        // Add old geometry as damage if shrinkage caused re-centering within bounds.
        if let Some(damage) = self.damage.as_mut().filter(|_| geometry.size < old_size) {
            let old_loc = damage.loc - old_size.sub(geometry.size).to_point();
            let old_rect = Rectangle::from_loc_and_size(old_loc, old_size);
            *damage = damage.merge(old_rect);
        }
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
    pub fn set_dimensions(&mut self, rectangle: Rectangle<i32, Logical>) {
        // Skip if we're already at the correct size.
        let current = self.transaction.as_ref().map_or(self.rectangle, |t| t.rectangle);
        if rectangle == current {
            return;
        }

        // Transactionally update geometry.
        self.start_transaction().rectangle = rectangle;

        // Send reconfigures after the initial commit.
        if self.initial_configure_sent {
            self.reconfigure();
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
    fn start_transaction(&mut self) -> &mut WindowTransaction {
        windows::start_transaction();
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
        !self.alive()
            || self.transaction.as_ref().map_or(true, |t| t.rectangle.size == self.acked_size)
    }

    /// Handle common surface commit logic for surfaces of any kind.
    pub fn surface_commit_common(&mut self, surface: &WlSurface, output: &Output) {
        // Cancel transactions on the commit after the configure was acked.
        self.acked_size = self.surface.acked_size();

        // Handle surface buffer changes.
        compositor::with_surface_tree_upward(
            surface,
            (),
            |_, data, _| {
                // Get access to surface data.
                data.data_map.insert_if_missing(|| RefCell::new(CatacombSurfaceData::new()));
                let mut surface_data =
                    data.data_map.get::<RefCell<CatacombSurfaceData>>().unwrap().borrow_mut();

                // Check if new buffer has been attached.
                let mut attributes = data.cached_state.current::<SurfaceAttributes>();
                let buffer_assignment = match attributes.buffer.take() {
                    Some(buffer_assignment) => buffer_assignment,
                    None => return TraversalAction::DoChildren(()),
                };

                // Update opaque regions.
                surface_data.opaque_region = attributes
                    .opaque_region
                    .as_mut()
                    .map(|region| mem::take(&mut region.rects))
                    .unwrap_or_default();

                // Store pending buffer updates.
                surface_data.update_buffer(&mut attributes, buffer_assignment);
                self.buffers_pending = true;

                TraversalAction::DoChildren(())
            },
            |_, _, _| (),
            |_, _, _| true,
        );

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
                    .get::<RefCell<CatacombSurfaceData>>()
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

    /// Close the application.
    pub fn kill(&mut self) {
        self.surface.send_close();
        self.dead = true;
    }

    /// Check if this window requires a redraw.
    pub fn damaged(&self) -> bool {
        self.damage.is_some()
    }

    /// Get pending window damage.
    pub fn damage(&self, canvas: &Canvas) -> Option<Rectangle<i32, Physical>> {
        // Offset damage by window bounds.
        let mut damage = self.damage?;
        damage.loc += self.bounds().loc;

        // Convert to physical coordinates.
        let physical = damage.to_physical(canvas.scale());

        // Clamp to output size.
        let canvas_size = canvas.size().to_physical(canvas.scale());
        physical.intersection(Rectangle::from_loc_and_size((0, 0), canvas_size))
    }

    /// Window's opaque region relative to its position.
    pub fn opaque_region(&self) -> impl Iterator<Item = Rectangle<i32, Logical>> + '_ {
        let bounds = self.bounds();
        self.opaque_region.iter().copied().map(move |mut rect| {
            rect.loc += bounds.loc;
            rect
        })
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
    pub fn surface_commit(&mut self, surface: &WlSurface, output: &mut Output) {
        self.update_layer_state(output);
        self.update_dimensions(output);
        self.surface_commit_common(surface, output);
    }

    /// Recompute the window's size and location.
    pub fn update_dimensions(&mut self, output: &mut Output) {
        let state = compositor::with_states(self.surface.surface(), |states| {
            *states.cached_state.current::<LayerSurfaceCachedState>()
        });

        // Exclude gesture handle from Top/Overlay window size.
        let output_size = match state.layer {
            Layer::Background | Layer::Bottom => output.size(),
            Layer::Top | Layer::Overlay => output.wm_size(),
        };
        let mut size = state.size;

        let exclusive = match state.exclusive_zone {
            ExclusiveZone::Neutral => *output.exclusive(),
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
        self.set_dimensions(dimensions);
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
        output.exclusive().reset(old_anchor, old_exclusive);
        output.exclusive().update(state.anchor, state.exclusive_zone, state.layer);
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
