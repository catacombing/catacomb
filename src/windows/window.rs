//! Wayland client window.

use std::cell::RefCell;
use std::mem;
use std::sync::Mutex;
use std::time::Instant;

use _presentation_time::wp_presentation_feedback::Kind as FeedbackKind;
use smithay::backend::drm::{DrmEventMetadata, DrmEventTime};
use smithay::backend::renderer::element::{RenderElementPresentationState, RenderElementStates};
use smithay::backend::renderer::gles2::Gles2Renderer;
use smithay::backend::renderer::{self, BufferType, ImportAll};
use smithay::reexports::wayland_protocols::wp::presentation_time::server as _presentation_time;
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
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface, XdgPopupSurfaceRoleAttributes};

use crate::drawing::{CatacombElement, CatacombSurfaceData, RenderTexture, Texture};
use crate::geometry::Vector;
use crate::output::{ExclusiveSpace, Output};
use crate::windows;
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};

/// Wayland client window state.
#[derive(Debug)]
pub struct Window<S = ToplevelSurface> {
    /// Initial size configure status.
    pub initial_configure_sent: bool,

    /// Last configure size acked by the client.
    pub acked_size: Size<i32, Logical>,

    /// Whether the window should be excluded for keyboard focus.
    pub deny_focus: bool,

    /// Attached surface.
    pub surface: S,

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
            initial_configure_sent: Default::default(),
            presentation_callbacks: Default::default(),
            texture_cache: Default::default(),
            transaction: Default::default(),
            deny_focus: Default::default(),
            acked_size: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
            popups: Default::default(),
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
        for popup in self.popups.iter().rev() {
            let popup_location = location + popup.bounds().loc.scale(window_scale);
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
                        let texture =
                            Texture::from_surface(texture, location, &data, surface_data, surface);
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
                surface_data.update_buffer(&mut attributes, buffer_assignment);

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
                    .map_or_else(Size::default, |data| data.borrow().size);
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
