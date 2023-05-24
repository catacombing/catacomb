//! Wayland client window.

use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::sync::Mutex;
use std::time::Instant;
use std::{cmp, mem};

use _presentation_time::wp_presentation_feedback::Kind as FeedbackKind;
use smithay::backend::drm::{DrmEventMetadata, DrmEventTime};
use smithay::backend::renderer::element::{RenderElementPresentationState, RenderElementStates};
use smithay::backend::renderer::gles::GlesRenderer;
use smithay::backend::renderer::{self, BufferType, ImportAll};
use smithay::reexports::wayland_protocols::wp::presentation_time::server as _presentation_time;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_positioner::{
    self, ConstraintAdjustment, Gravity,
};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Physical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::presentation::{
    PresentationFeedbackCachedState, PresentationFeedbackCallback,
};
use smithay::wayland::shell::wlr_layer::{
    Anchor, ExclusiveZone, KeyboardInteractivity, Layer, LayerSurfaceCachedState,
};
use smithay::wayland::shell::xdg::{
    PopupSurface, PositionerState, ToplevelSurface, XdgPopupSurfaceRoleAttributes,
    XdgToplevelSurfaceRoleAttributes,
};

use crate::drawing::{CatacombElement, CatacombSurfaceData, RenderTexture, Texture};
use crate::geometry::Vector;
use crate::output::{ExclusiveSpace, Output};
use crate::windows;
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};

/// Wayland client window state.
#[derive(Debug)]
pub struct Window<S = ToplevelSurface> {
    /// Last configure size acked by the client.
    pub acked_size: Size<i32, Logical>,

    /// Whether the window should be excluded for keyboard focus.
    pub deny_focus: bool,

    /// Attached surface.
    pub surface: S,

    /// Application ID.
    pub app_id: Option<String>,

    /// Buffers pending to be updated.
    dirty: bool,

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

    /// Pending wp_presentation callbacks.
    presentation_callbacks: Vec<PresentationCallback>,

    /// Window liveliness override.
    dead: bool,
}

impl<S: Surface + 'static> Window<S> {
    /// Create a new Toplevel window.
    pub fn new(surface: S) -> Self {
        Window {
            surface,
            presentation_callbacks: Default::default(),
            texture_cache: Default::default(),
            transaction: Default::default(),
            deny_focus: Default::default(),
            acked_size: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
            popups: Default::default(),
            app_id: Default::default(),
            dirty: Default::default(),
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

    /// Check window liveliness.
    pub fn alive(&self) -> bool {
        !self.dead && self.surface.alive()
    }

    /// Check if this window contains a specific point.
    pub fn contains(&self, point: Point<f64, Logical>) -> bool {
        self.bounds().to_f64().contains(point)
    }

    /// Add this window's textures to the supplied buffer.
    pub fn textures(
        &self,
        textures: &mut Vec<CatacombElement>,
        output_scale: f64,
        window_scale: impl Into<Option<f64>>,
        location: impl Into<Option<Point<i32, Logical>>>,
    ) {
        self.textures_internal(textures, output_scale, window_scale, location, None);
    }

    /// Internal method for getting render textures.
    ///
    /// This ensures both toplevels and their popup children are clamped to the
    /// toplevel's target geometry.
    fn textures_internal(
        &self,
        textures: &mut Vec<CatacombElement>,
        output_scale: f64,
        window_scale: impl Into<Option<f64>>,
        location: impl Into<Option<Point<i32, Logical>>>,
        bounds: impl Into<Option<Rectangle<i32, Physical>>>,
    ) {
        let location = location.into().unwrap_or_else(|| self.bounds().loc);
        let window_scale = window_scale.into().unwrap_or(1.);

        // Calculate window bounds only for the toplevel window.
        let bounds = bounds.into().unwrap_or_else(|| {
            let mut bounds = self.bounds().to_physical_precise_round(output_scale);
            bounds.loc = location.to_physical_precise_round(output_scale);
            bounds
        });

        // Add popup textures.
        let logical_bounds = bounds.to_f64().to_logical(output_scale).to_i32_round();
        for popup in self.popups.iter().rev() {
            let unconstrained_location = location + popup.bounds().loc.scale(window_scale);
            let popup_location = popup.constrain_location(unconstrained_location, logical_bounds);

            popup.textures_internal(
                textures,
                output_scale,
                window_scale,
                Some(popup_location),
                bounds,
            );
        }

        // Add windows' textures.
        let physical_location = location.to_physical_precise_round(output_scale);
        for texture in self.texture_cache.textures.iter().rev() {
            CatacombElement::add_element(
                textures,
                texture.clone(),
                physical_location,
                bounds,
                window_scale,
                output_scale,
            );
        }
    }

    /// Default window location and size.
    fn bounds(&self) -> Rectangle<i32, Logical> {
        // Center window inside its space.
        let mut bounds = self.rectangle;
        bounds.loc += self.internal_offset();
        bounds
    }

    /// Internal window offset for centering the window inside its space.
    pub fn internal_offset(&self) -> Point<i32, Logical> {
        let cache_size = self.texture_cache.size();
        let bounds = self.rectangle;
        let x = ((bounds.size.w - cache_size.w) / 2).max(0);
        let y = ((bounds.size.h - cache_size.h) / 2).max(0);
        (x, y).into()
    }

    /// Import the buffers of all surfaces into the renderer.
    pub fn import_buffers(&mut self, renderer: &mut GlesRenderer) {
        // Do not import buffers during a transaction.
        if self.transaction.is_some() {
            return;
        }

        // Import buffers for all popup windows.
        for popup in &mut self.popups {
            popup.import_buffers(renderer);
        }

        // Short-circuit if we know no new buffer is waiting for import.
        if !mem::take(&mut self.dirty) {
            return;
        }

        self.texture_cache.requested_size = self.rectangle.size;
        self.texture_cache.textures.clear();

        let geometry = self.surface.geometry();

        compositor::with_surface_tree_upward(
            self.surface.surface(),
            Point::from((0, 0)) - geometry.unwrap_or_default().loc,
            |surface, surface_data, location| {
                let data = match surface_data.data_map.get::<RefCell<CatacombSurfaceData>>() {
                    Some(data) => data,
                    None => return TraversalAction::SkipChildren,
                };
                let mut data = data.borrow_mut();

                // Use the subsurface's location as the origin for its children.
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    // NOTE: Use current after smithay/smithay#883 is fixed.
                    let subsurface = surface_data.cached_state.pending::<SubsurfaceCachedState>();
                    location += subsurface.location;
                }

                // Skip surface if buffer was already imported.
                if let Some(texture) = &data.texture {
                    self.texture_cache.push(texture.clone(), location);
                    return TraversalAction::DoChildren(location);
                }

                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                // Stage presentation callbacks for submission.
                let mut feedback_state =
                    surface_data.cached_state.current::<PresentationFeedbackCachedState>();
                for callback in feedback_state.callbacks.drain(..) {
                    let callback = PresentationCallback::new(surface.clone(), callback);
                    self.presentation_callbacks.push(callback);
                }

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
                        let texture = Texture::from_surface(texture, location, &data, surface);
                        let render_texture = RenderTexture::new(texture);
                        self.texture_cache.push(render_texture.clone(), location);
                        data.texture = Some(render_texture);

                        TraversalAction::DoChildren(location)
                    },
                    _ => {
                        eprintln!("unable to import buffer");
                        data.buffer = None;

                        TraversalAction::SkipChildren
                    },
                };

                // Clear buffer damage after successful import.
                data.damage.clear();

                action
            },
            |_, _, _| (),
            |_, _, _| true,
        );
    }

    /// Check whether there is a new buffer pending with an updated geometry
    /// size.
    pub fn pending_buffer_resize(&self) -> bool {
        self.rectangle.size != self.texture_cache.requested_size
    }

    /// Mark all rendered clients as presented for `wp_presentation`.
    pub fn mark_presented(
        &mut self,
        states: &RenderElementStates,
        metadata: &Option<DrmEventMetadata>,
        output: &Output,
        start_time: &Instant,
    ) {
        // Skip presentation feedback during transactions.
        if self.transaction.is_some() {
            return;
        }

        let refresh = output.frame_interval().as_nanos() as u32;
        let output = output.smithay_output();

        // Try to get monitor clock.
        let time = metadata.as_ref().and_then(|metadata| match metadata.time {
            DrmEventTime::Monotonic(time) => Some(time),
            DrmEventTime::Realtime(_) => None,
        });
        let seq = metadata.as_ref().map(|metadata| metadata.sequence).unwrap_or(0);

        // Use Monitor clock or fallback to internal time since startup.
        let (time, flags) = match time {
            Some(time) => {
                (time, FeedbackKind::Vsync | FeedbackKind::HwClock | FeedbackKind::HwCompletion)
            },
            None => (start_time.elapsed(), FeedbackKind::Vsync),
        };

        for PresentationCallback { callback, surface } in self.presentation_callbacks.drain(..) {
            // Set zero-copy flag if direct scanout was used.
            let zero_copy = states.element_render_state(&surface).map_or(false, |states| {
                states.presentation_state == RenderElementPresentationState::ZeroCopy
            });
            let flags = if zero_copy { FeedbackKind::ZeroCopy } else { flags };

            callback.presented(output, time, refresh, seq as u64, flags);
        }
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

        // Apply the dimensional changes.
        self.surface.resize(rectangle.size);
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
    pub fn surface_commit_common(&mut self, surface: &WlSurface) {
        // Cancel transactions on the commit after the configure was acked.
        self.acked_size = self.surface.acked_size();

        // Handle surface buffer changes.
        compositor::with_surface_tree_upward(
            surface,
            (),
            |_, data, _| {
                // Request buffer update.
                self.dirty = true;

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
                surface_data.update_buffer(data, &mut attributes, buffer_assignment);

                TraversalAction::DoChildren(())
            },
            |_, _, _| (),
            |_, _, _| true,
        );

        // Send initial configure after the first commit.
        self.surface.initial_configure();

        // Update App ID.
        compositor::with_states(self.surface.surface(), |states| {
            let attrs = states
                .data_map
                .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                .and_then(|attributes| attributes.lock().ok());

            if let Some(attributes) = attrs.filter(|attrs| attrs.app_id != self.app_id) {
                self.app_id = attributes.app_id.clone();
            }
        })
    }

    /// Find subsurface at the specified location.
    pub fn surface_at(&self, position: Point<f64, Logical>) -> Option<OffsetSurface> {
        // Check popups top to bottom first.
        let parent_offset = self.rectangle.loc;
        let relative = position - parent_offset.to_f64();
        let popup = self.popups.iter().find_map(|popup| popup.surface_at(relative));
        if let Some(mut popup_surface) = popup {
            // Convert surface offset from parent-relative to global.
            popup_surface.offset += parent_offset;
            return Some(popup_surface);
        }

        let geometry = self.surface.geometry().unwrap_or_default();
        let bounds = self.bounds();

        let result = RefCell::new(None);
        compositor::with_surface_tree_upward(
            self.surface.surface(),
            bounds.loc,
            |wl_surface, surface_data, location| {
                let mut location = *location;
                if surface_data.role == Some("subsurface") {
                    // NOTE: Use current after smithay/smithay#883 is fixed.
                    let current = surface_data.cached_state.pending::<SubsurfaceCachedState>();
                    location += current.location;
                }

                // Calculate surface's bounding box.
                //
                // This includes content outside of the surface's geometry.
                let size = surface_data
                    .data_map
                    .get::<RefCell<CatacombSurfaceData>>()
                    .map_or_else(Size::default, |data| data.borrow().dst_size);
                let surface_loc = location - geometry.loc;
                let surface_rect =
                    Rectangle::from_loc_and_size(surface_loc.to_f64(), size.to_f64());

                // Check if the position is within the surface bounds.
                if surface_rect.contains(position) {
                    let surface = OffsetSurface::new(wl_surface.clone(), surface_loc);
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
    pub fn popup_surface_commit(&mut self, root_surface: &WlSurface, surface: &WlSurface) -> bool {
        self.popups.iter_mut().any(|window| {
            if window.surface.surface() == root_surface {
                window.surface_commit_common(surface);
                window.rectangle.loc = window.position();
                return true;
            }

            window.popup_surface_commit(root_surface, surface)
        })
    }

    /// Check if window owns a surface.
    pub fn owns_surface(&self, surface: &WlSurface) -> bool {
        let mut owns_surface = false;
        self.with_surfaces(|window_surface, _| owns_surface |= window_surface == surface);
        owns_surface
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
    pub fn dirty(&self) -> bool {
        self.dirty && self.transaction.is_none()
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

    /// Calculate constrained popup location.
    fn constrain_location(
        &self,
        location: Point<i32, Logical>,
        bounds: Rectangle<i32, Logical>,
    ) -> Point<i32, Logical> {
        let size = self.texture_cache.size();
        self.surface.with_pending_state(|state| {
            PopupConstrainer(&mut state.positioner).constrain(bounds, location, size)
        })
    }
}

impl Window<CatacombLayerSurface> {
    /// Handle a surface commit for layer shell windows.
    pub fn surface_commit(
        &mut self,
        surface: &WlSurface,
        output: &mut Output,
        fullscreen_active: bool,
    ) {
        self.update_layer_state(output);
        self.update_dimensions(output, fullscreen_active);
        self.surface_commit_common(surface);
    }

    /// Recompute the window's size and location.
    pub fn update_dimensions(&mut self, output: &mut Output, fullscreen_active: bool) {
        let state = compositor::with_states(self.surface.surface(), |states| {
            *states.cached_state.current::<LayerSurfaceCachedState>()
        });

        // Exclude gesture handle from Top/Overlay window size.
        let output_size = match state.layer {
            Layer::Overlay if fullscreen_active => output.size(),
            _ => output.wm_size(),
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
pub struct TextureCache {
    /// Window's reported size.
    geometry_size: Option<Size<i32, Logical>>,

    /// Size of all combined textures.
    texture_size: Size<i32, Logical>,

    /// Desired window size during import.
    requested_size: Size<i32, Logical>,

    /// Cached textures.
    textures: Vec<RenderTexture>,
}

impl TextureCache {
    /// Add a new texture.
    fn push(&mut self, texture: RenderTexture, location: Point<i32, Logical>) {
        // Update the combined texture size.
        let max_size = texture.size() + location.max((0, 0)).to_size();
        self.texture_size = self.texture_size.max(max_size);

        self.textures.push(texture);
    }

    /// Get the size of the cached texture.
    fn size(&self) -> Size<i32, Logical> {
        self.geometry_size.unwrap_or(self.texture_size)
    }
}

/// Surface callback for wp_presentation_time.
#[derive(Debug)]
struct PresentationCallback {
    callback: PresentationFeedbackCallback,
    surface: WlSurface,
}

impl PresentationCallback {
    fn new(surface: WlSurface, callback: PresentationFeedbackCallback) -> Self {
        Self { callback, surface }
    }
}

/// Helper for applying constraints to popup locations.
struct PopupConstrainer<'a>(&'a mut PositionerState);

impl<'a> PopupConstrainer<'a> {
    /// Get a popup's constrained location.
    fn constrain(
        &mut self,
        bounds: Rectangle<i32, Logical>,
        mut location: Point<i32, Logical>,
        size: Size<i32, Logical>,
    ) -> Point<i32, Logical> {
        // Skip if no constraint is necessary.
        let rect = Rectangle::from_loc_and_size(location, size);
        if bounds.contains_rect(rect) {
            return location;
        }

        // Store `self` accesses so we can borrow it to `flip` closure.
        let flip_y = self.constraint_adjustment.contains(ConstraintAdjustment::FlipY);
        let flip_x = self.constraint_adjustment.contains(ConstraintAdjustment::FlipX);
        let old_anchor = self.anchor_edges;
        let old_gravity = self.gravity;

        // Flip anchor and gravity if axis requires constraining.
        let mut flip = |bounds_loc, bounds_size, loc, size, x_axis| {
            if loc < bounds_loc || loc + size > bounds_loc + bounds_size {
                self.anchor_edges = Self::flip_anchor(self.anchor_edges, x_axis);
                self.gravity = Self::flip_gravity(self.gravity, x_axis);
            }
        };

        // Slide single dimension to be within bounds.
        //
        // The protocol specification's language is over-complicated, but boils
        // down to just moving the edge outside of the bounds inwards until the
        // opposite edge touches the opposing bounds. If both sides are outside
        // of the bounds from the start, it is a no-op.
        let slide = |bounds_loc, bounds_size, loc, size| {
            if loc < bounds_loc && loc + size < bounds_loc + bounds_size {
                cmp::min(bounds_loc + bounds_size - size, bounds_loc)
            } else if loc > bounds_loc && loc + size > bounds_loc + bounds_size {
                cmp::max(bounds_loc + bounds_size - size, bounds_loc)
            } else {
                loc
            }
        };

        // Attempt flip constraint resolution.
        if flip_x {
            flip(bounds.loc.x, bounds.size.w, location.x, size.w, true);
        }
        if flip_y {
            flip(bounds.loc.y, bounds.size.h, location.y, size.h, false);
        }

        // Return new location if flip solved our constraint violations.
        let new_location = self.get_geometry().loc;
        let new_rect = Rectangle::from_loc_and_size(new_location, size);
        if bounds.contains_rect(new_rect) {
            return new_location;
        }

        // Revert flip transforms if they didn't resolve the constraint violation.
        self.anchor_edges = old_anchor;
        self.gravity = old_gravity;

        // Attempt slide constraint resolution.
        if self.constraint_adjustment.contains(ConstraintAdjustment::SlideX) {
            location.x = slide(bounds.loc.x, bounds.size.w, location.x, size.w);
        }
        if self.constraint_adjustment.contains(ConstraintAdjustment::SlideY) {
            location.y = slide(bounds.loc.y, bounds.size.h, location.y, size.h);
        }

        location
    }

    /// Flip an anchor on one axis.
    fn flip_anchor(anchor: xdg_positioner::Anchor, x_axis: bool) -> xdg_positioner::Anchor {
        match (anchor, x_axis) {
            (xdg_positioner::Anchor::Right, true) => xdg_positioner::Anchor::Left,
            (xdg_positioner::Anchor::TopRight, true) => xdg_positioner::Anchor::TopLeft,
            (xdg_positioner::Anchor::BottomRight, true) => xdg_positioner::Anchor::BottomLeft,
            (xdg_positioner::Anchor::Left, true) => xdg_positioner::Anchor::Right,
            (xdg_positioner::Anchor::TopLeft, true) => xdg_positioner::Anchor::TopRight,
            (xdg_positioner::Anchor::BottomLeft, true) => xdg_positioner::Anchor::BottomRight,
            (xdg_positioner::Anchor::Top, false) => xdg_positioner::Anchor::Bottom,
            (xdg_positioner::Anchor::TopRight, false) => xdg_positioner::Anchor::BottomRight,
            (xdg_positioner::Anchor::TopLeft, false) => xdg_positioner::Anchor::BottomLeft,
            (xdg_positioner::Anchor::Bottom, false) => xdg_positioner::Anchor::Top,
            (xdg_positioner::Anchor::BottomRight, false) => xdg_positioner::Anchor::TopRight,
            (xdg_positioner::Anchor::BottomLeft, false) => xdg_positioner::Anchor::TopLeft,
            (anchor, _) => anchor,
        }
    }

    /// Flip a gravity on one axis.
    fn flip_gravity(gravity: Gravity, x_axis: bool) -> Gravity {
        match (gravity, x_axis) {
            (Gravity::Right, true) => Gravity::Left,
            (Gravity::TopRight, true) => Gravity::TopLeft,
            (Gravity::BottomRight, true) => Gravity::BottomLeft,
            (Gravity::Left, true) => Gravity::Right,
            (Gravity::TopLeft, true) => Gravity::TopRight,
            (Gravity::BottomLeft, true) => Gravity::BottomRight,
            (Gravity::Top, false) => Gravity::Bottom,
            (Gravity::TopRight, false) => Gravity::BottomRight,
            (Gravity::TopLeft, false) => Gravity::BottomLeft,
            (Gravity::Bottom, false) => Gravity::Top,
            (Gravity::BottomRight, false) => Gravity::TopRight,
            (Gravity::BottomLeft, false) => Gravity::TopLeft,
            (gravity, _) => gravity,
        }
    }
}

impl<'a> Deref for PopupConstrainer<'a> {
    type Target = PositionerState;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> DerefMut for PopupConstrainer<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positioner_constraints_slide() {
        let mut positioner = dummy_positioner();

        // Slide left.
        positioner.constraint_adjustment =
            ConstraintAdjustment::SlideX | ConstraintAdjustment::SlideY;
        positioner.offset = (95, 95).into();
        assert_eq!(constrain(&mut positioner), (90, 90).into());

        // Slide right.
        positioner.offset = (-5, -5).into();
        assert_eq!(constrain(&mut positioner), (0, 0).into());

        // No-op because both sides out of bounds.
        positioner.rect_size = (1000, 1000).into();
        positioner.offset = (-100, -100).into();
        assert_eq!(constrain(&mut positioner), (-100, -100).into());
    }

    // Get a dummy positioner.
    fn dummy_positioner() -> PositionerState {
        let mut positioner = PositionerState::default();
        positioner.rect_size = (10, 10).into();
        positioner.anchor_rect = Rectangle::from_loc_and_size((0, 0), (10, 10));
        positioner.anchor_edges = xdg_positioner::Anchor::TopLeft;
        positioner.gravity = Gravity::BottomRight;
        positioner.constraint_adjustment = ConstraintAdjustment::None;
        positioner
    }

    fn constrain(positioner: &mut PositionerState) -> Point<i32, Logical> {
        PopupConstrainer(&mut positioner.clone()).constrain(
            Rectangle::from_loc_and_size((0, 0), (100, 100)),
            positioner.offset,
            positioner.rect_size,
        )
    }
}
