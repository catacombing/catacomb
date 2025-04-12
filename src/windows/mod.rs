//! Window management.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::mem;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant, UNIX_EPOCH};

use _decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;
use catacomb_ipc::{AppIdMatcher, WindowScale};
use smithay::backend::drm::DrmEventMetadata;
use smithay::backend::renderer::element::{Element, RenderElementStates};
use smithay::backend::renderer::gles::GlesRenderer;
use smithay::reexports::calloop::LoopHandle;
use smithay::reexports::wayland_protocols::xdg::decoration as _decoration;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::State;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{Logical, Point, Rectangle};
use smithay::wayland::compositor;
use smithay::wayland::session_lock::LockSurface;
use smithay::wayland::shell::wlr_layer::{Layer, LayerSurface};
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface};

use crate::catacomb::Catacomb;
use crate::drawing::{CatacombElement, Graphics};
use crate::input::{HandleGesture, TouchState};
use crate::layer::Layers;
use crate::orientation::Orientation;
use crate::output::{Canvas, Output, GESTURE_HANDLE_HEIGHT};
use crate::overview::{DragActionType, DragAndDrop, Overview};
use crate::windows::layout::{LayoutPosition, Layouts};
use crate::windows::surface::{CatacombLayerSurface, InputSurface, InputSurfaceKind, Surface};
use crate::windows::window::Window;

pub mod layout;
pub mod surface;
pub mod window;

/// Transaction timeout for locked sessions.
const MAX_LOCKED_TRANSACTION_MILLIS: u64 = 100;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_MILLIS: u64 = 1000;

/// Global transaction timer in milliseconds.
static TRANSACTION_START: AtomicU64 = AtomicU64::new(0);

/// Start a new transaction.
///
/// This will reset the transaction start to the current system time if there's
/// no transaction pending, setting up the timeout for the transaction.
pub fn start_transaction() {
    // Skip when transaction is already active.
    if TRANSACTION_START.load(Ordering::Relaxed) != 0 {
        return;
    }

    let now = UNIX_EPOCH.elapsed().unwrap().as_millis() as u64;
    TRANSACTION_START.store(now, Ordering::Relaxed);
}

/// Perform operation for every known window.
macro_rules! with_all_windows {
    ($windows:expr, |$window:tt| $fn:expr) => {{
        match $windows.pending_view() {
            View::Lock(Some($window)) => $fn,
            _ => (),
        }

        for $window in $windows.layouts.windows() {
            $fn
        }
        for $window in $windows.layers.iter() {
            $fn
        }
    }};
}

/// Perform mutable operation for every known window.
macro_rules! with_all_windows_mut {
    ($windows:expr, |$window:tt| $fn:expr) => {{
        match $windows.pending_view_mut() {
            View::Lock(Some($window)) => $fn,
            _ => (),
        }

        for mut $window in $windows.layouts.windows_mut() {
            $fn
        }
        for $window in $windows.layers.iter_mut() {
            $fn
        }
    }};
}

/// Container tracking all known clients.
#[derive(Debug)]
pub struct Windows {
    pub window_scales: Vec<(AppIdMatcher, WindowScale)>,

    orphan_popups: Vec<Window<PopupSurface>>,
    layouts: Layouts,
    layers: Layers,
    view: View,

    event_loop: LoopHandle<'static, Catacomb>,
    activated: Option<ToplevelSurface>,
    transaction: Option<Transaction>,
    textures: Vec<CatacombElement>,
    start_time: Instant,
    output: Output,

    /// Cached output state for rendering.
    ///
    /// This is used to tie the transactions to their respective output size and
    /// should be passed to anyone who doesn't communicate with clients about
    /// future updates, but instead tries to calculate things for the next
    /// rendered frame.
    canvas: Canvas,

    /// Orientation independent from [`Windows::orientation_locked`] state.
    unlocked_orientation: Orientation,
    orientation_locked: bool,

    /// IME force enable/disable state.
    ime_override: Option<bool>,

    /// Client-independent damage.
    dirty: bool,
}

impl Windows {
    pub fn new(display: &DisplayHandle, event_loop: LoopHandle<'static, Catacomb>) -> Self {
        let output = Output::new_dummy(display);
        let canvas = *output.canvas();

        Self {
            event_loop,
            output,
            canvas,
            start_time: Instant::now(),
            orientation_locked: true,
            dirty: true,
            unlocked_orientation: Default::default(),
            orphan_popups: Default::default(),
            window_scales: Default::default(),
            ime_override: Default::default(),
            transaction: Default::default(),
            activated: Default::default(),
            textures: Default::default(),
            layouts: Default::default(),
            layers: Default::default(),
            view: Default::default(),
        }
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface) {
        // Set general window states.
        surface.with_pending_state(|state| {
            // Mark window as tiled, using maximized fallback if tiling is unsupported.
            if surface.version() >= 2 {
                state.states.set(State::TiledBottom);
                state.states.set(State::TiledRight);
                state.states.set(State::TiledLeft);
                state.states.set(State::TiledTop);
            } else {
                state.states.set(State::Maximized);
            }

            // Always use server-side decoration.
            state.decoration_mode = Some(DecorationMode::ServerSide);

            // Activate by default to avoid reconfigure.
            state.states.set(State::Activated);
        });

        let transform = self.output.orientation().surface_transform();
        let scale = self.output.scale();

        let window = Rc::new(RefCell::new(Window::new(&[], surface, scale, transform, None)));
        self.layouts.create(&self.output, window);

        // Leave fullscreen if it is currently active.
        self.unfullscreen(None);
    }

    /// Add a new layer shell window.
    pub fn add_layer(&mut self, layer: Layer, surface: LayerSurface, namespace: String) {
        let transform = self.output.orientation().surface_transform();
        let scale = self.output.scale();

        let surface = CatacombLayerSurface::new(layer, surface);
        let mut window =
            Window::new(&self.window_scales, surface, scale, transform, Some(namespace));

        window.enter(&self.output);
        self.layers.add(window);
    }

    /// Add a new popup window.
    pub fn add_popup(&mut self, popup: PopupSurface) {
        let transform = self.output.orientation().surface_transform();
        let scale = self.output.scale();

        self.orphan_popups.push(Window::new(&[], popup, scale, transform, None));
    }

    /// Move popup location.
    pub fn reposition_popup(&mut self, popup: &PopupSurface, token: u32) {
        for mut window in self.layouts.windows_mut() {
            let scale = self.output.scale();
            window.reposition_popup(scale, popup, token);
        }
    }

    /// Update the session lock surface.
    pub fn set_lock_surface(&mut self, surface: LockSurface) {
        let surface_transform = self.output.orientation().surface_transform();
        let output_scale = self.output.scale();
        let output_size = self.output.size();

        let lock_window = match self.pending_view_mut() {
            View::Lock(lock_window) => lock_window,
            _ => return,
        };

        // Set lock surface size.
        let mut window = Window::new(&[], surface, output_scale, surface_transform, None);
        window.set_dimensions(output_scale, Rectangle::from_size(output_size));

        // Update lockscreen.
        *lock_window = Some(Box::new(window));
    }

    /// Lock the session.
    pub fn lock(&mut self) {
        self.set_view(View::Lock(None));
    }

    /// Unlock the session.
    pub fn unlock(&mut self) {
        self.start_transaction().view = Some(View::Workspace);
    }

    /// Find the XDG shell window responsible for a specific surface.
    pub fn find_xdg(&mut self, wl_surface: &WlSurface) -> Option<RefMut<Window>> {
        // Get root surface.
        let mut wl_surface = Cow::Borrowed(wl_surface);
        while let Some(surface) = compositor::get_parent(&wl_surface) {
            wl_surface = Cow::Owned(surface);
        }

        self.layouts.windows_mut().find(|window| window.surface().eq(wl_surface.as_ref()))
    }

    /// Handle a surface commit for any window.
    pub fn surface_commit(&mut self, surface: &WlSurface) {
        // Get the topmost surface for window comparison.
        let mut root_surface = Cow::Borrowed(surface);
        while let Some(parent) = compositor::get_parent(&root_surface) {
            root_surface = Cow::Owned(parent);
        }

        // Find a window matching the root surface.
        macro_rules! find_window {
            ($windows:expr) => {{
                $windows.find(|window| window.surface().eq(root_surface.as_ref()))
            }};
        }

        // Handle session lock surface commits.
        let scale = self.output.scale();
        if let View::Lock(Some(window)) = self.pending_view_mut() {
            if window.surface() == root_surface.as_ref() {
                window.surface_commit_common(scale, &[], surface);
                return;
            }
        }

        // Handle XDG surface commits.
        if let Some(mut window) = find_window!(self.layouts.windows_mut()) {
            window.surface_commit_common(scale, &self.window_scales, surface);
            return;
        }

        // Handle popup orphan adoption.
        self.orphan_surface_commit(&root_surface);

        // Apply popup surface commits.
        for mut window in self.layouts.windows_mut() {
            if window.popup_surface_commit(scale, &root_surface, surface) {
                // Abort as soon as we found the parent.
                return;
            }
        }

        // Abort if we can't find any window for this surface.
        let window = match find_window!(self.layers.iter_mut()) {
            Some(window) => window,
            None => return,
        };

        // Handle layer shell surface commits.
        let old_exclusive = *self.output.exclusive();
        let fullscreen_active = matches!(self.view, View::Fullscreen(_));
        window.surface_commit(&self.window_scales, &mut self.output, fullscreen_active, surface);

        // Resize windows after exclusive zone changes.
        if self.output.exclusive() != &old_exclusive {
            self.resize_visible();
        }
    }

    /// Handle orphan popup surface commits.
    ///
    /// After the first surface commit, every popup should have a parent set.
    /// This function puts it at the correct location in the window tree
    /// below its parent.
    ///
    /// Popups will be dismissed if a surface commit is made for them without
    /// any parent set. They will also be dismissed if the parent is not
    /// currently visible.
    pub fn orphan_surface_commit(&mut self, root_surface: &WlSurface) -> Option<()> {
        let mut orphans = self.orphan_popups.iter();
        let index = orphans.position(|popup| popup.surface() == root_surface)?;
        let mut popup = self.orphan_popups.swap_remove(index);
        let parent = popup.parent()?;

        // Try and add it to the primary window.
        let active_layout = self.layouts.active();
        if let Some(primary) = active_layout.primary().as_ref() {
            popup = primary.borrow_mut().add_popup(popup, &parent)?;
        }

        // Try and add it to the secondary window.
        if let Some(secondary) = active_layout.secondary().as_ref() {
            popup = secondary.borrow_mut().add_popup(popup, &parent)?;
        }

        // Dismiss popup if it wasn't added to either of the visible windows.
        popup.surface.send_popup_done();

        Some(())
    }

    /// Import pending buffers for all windows.
    #[cfg_attr(feature = "profiling", profiling::function)]
    pub fn import_buffers(&mut self, renderer: &mut GlesRenderer) {
        // Import XDG windows/popups.
        for mut window in self.layouts.windows_mut() {
            window.import_buffers(renderer);
        }

        // Import layer shell windows.
        for window in self.layers.iter_mut() {
            window.import_buffers(renderer);
        }

        // Import session lock surface.
        if let View::Lock(Some(window)) = &mut self.view {
            window.import_buffers(renderer);
        }
    }

    /// Get all textures for rendering.
    #[cfg_attr(feature = "profiling", profiling::function)]
    pub fn textures(
        &mut self,
        renderer: &mut GlesRenderer,
        graphics: &mut Graphics,
        cursor_position: Option<Point<f64, Logical>>,
    ) -> &[CatacombElement] {
        // Clear global damage.
        self.dirty = false;

        let scale = self.output.scale();
        self.textures.clear();

        // Draw gesture handle when not in fullscreen/lock view.
        if !matches!(self.view, View::Fullscreen(_) | View::Lock(_)) {
            // Get texture for gesture handle.
            let gesture_handle = graphics.gesture_handle(renderer, &self.canvas, self.ime_override);

            // Calculate gesture handle bounds.
            let mut bounds = gesture_handle.geometry(scale.into());

            // Calculate position for gesture handle.
            let output_height = self.canvas.physical_size().h;
            let handle_location = (0, output_height - bounds.size.h);
            bounds.loc = handle_location.into();

            CatacombElement::add_element(
                &mut self.textures,
                gesture_handle,
                handle_location,
                bounds,
                None,
                scale,
            );
        }

        // Render touch location cursor.
        if let Some(cursor_position) = cursor_position {
            let cursor = graphics.cursor(renderer, &self.canvas);

            // Center texture around touch position.
            let mut cursor_position = cursor_position.to_physical(scale).to_i32_round();
            let mut bounds = cursor.geometry(scale.into());
            cursor_position.x -= bounds.size.w / 2;
            cursor_position.y -= bounds.size.h / 2;
            bounds.loc = cursor_position;

            CatacombElement::add_element(
                &mut self.textures,
                cursor,
                cursor_position,
                bounds,
                None,
                scale,
            );
        }

        match &mut self.view {
            View::Workspace => {
                for layer in self.layers.foreground() {
                    layer.textures(&mut self.textures, scale, None, None);
                }

                self.layouts.textures(&mut self.textures, scale);

                for layer in self.layers.background() {
                    layer.textures(&mut self.textures, scale, None, None);
                }
            },
            View::DragAndDrop(dnd) => {
                dnd.textures(&mut self.textures, &self.canvas, graphics);

                self.layouts.textures(&mut self.textures, scale);

                for layer in self.layers.background() {
                    layer.textures(&mut self.textures, scale, None, None);
                }
            },
            View::Overview(overview) => {
                overview.textures(
                    &mut self.textures,
                    &self.output,
                    &self.canvas,
                    &self.layouts,
                    graphics,
                );

                for layer in self.layers.background() {
                    layer.textures(&mut self.textures, scale, None, None);
                }
            },
            View::Fullscreen(window) => {
                for layer in self.layers.overlay() {
                    layer.textures(&mut self.textures, scale, None, None);
                }

                window.borrow().textures(&mut self.textures, scale, None, None);
            },
            View::Lock(Some(window)) => window.textures(&mut self.textures, scale, None, None),
            View::Lock(None) => (),
        }

        self.textures.as_slice()
    }

    /// Request new frames for all visible windows.
    pub fn request_frames(&mut self) {
        let runtime = self.runtime();

        match &self.view {
            View::Fullscreen(window) => {
                for overlay in self.layers.overlay() {
                    overlay.request_frame(runtime);
                }
                window.borrow().request_frame(runtime);
            },
            View::Overview(_) | View::DragAndDrop(_) => {
                for window in self.layers.background() {
                    window.request_frame(runtime);
                }
            },
            View::Workspace => {
                self.layers.request_frames(runtime);
                self.layouts.with_visible(|window| window.request_frame(runtime));
            },
            View::Lock(Some(window)) => window.request_frame(runtime),
            View::Lock(None) => (),
        }
    }

    /// Mark all rendered clients as presented for `wp_presentation`.
    pub fn mark_presented(
        &mut self,
        states: &RenderElementStates,
        metadata: &Option<DrmEventMetadata>,
    ) {
        // Update XDG client presentation time.
        for mut window in self.layouts.windows_mut() {
            window.mark_presented(states, metadata, &self.output, &self.start_time);
        }

        // Update layer-shell client presentation time.
        for layer in self.layers.iter_mut() {
            layer.mark_presented(states, metadata, &self.output, &self.start_time);
        }
    }

    /// Stage dead XDG shell window for reaping.
    pub fn reap_xdg(&mut self, surface: &ToplevelSurface) {
        // Remove fullscreen if this client was fullscreened.
        self.unfullscreen(surface);

        // Reap layout windows.
        self.layouts.reap(&self.output, surface);
    }

    /// Stage dead layer shell window for reaping.
    pub fn reap_layer(&mut self, surface: &LayerSurface) {
        // Start transaction to ensure window is reaped even without any resize.
        start_transaction();

        // Handle layer shell death.
        let old_exclusive = *self.output.exclusive();
        if let Some(window) = self.layers.iter().find(|layer| layer.surface.eq(surface)) {
            self.output.exclusive().reset(window.surface.anchor, window.surface.exclusive_zone);
        }

        // Resize windows if reserved layer space changed.
        if self.output.exclusive() != &old_exclusive {
            self.resize_visible();
        }
    }

    /// Reap dead XDG popup windows.
    pub fn refresh_popups(&mut self) {
        for mut window in self.layouts.windows_mut() {
            window.refresh_popups();
        }
    }

    /// Start Overview window Drag & Drop.
    pub fn start_dnd(&mut self, layout_position: LayoutPosition) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            _ => return,
        };

        // Convert layout position to window.
        let window = match self.layouts.window_at(layout_position) {
            Some(window) => window.clone(),
            None => return,
        };

        let dnd = DragAndDrop::new(&self.output, overview, layout_position, window);
        self.set_view(View::DragAndDrop(dnd));
    }

    /// Fullscreen the supplied XDG surface.
    pub fn fullscreen(&mut self, surface: &ToplevelSurface) {
        if let Some(window) = self.layouts.find_window(surface.surface()) {
            // Update window's XDG state.
            window.borrow_mut().surface.set_state(|state| {
                state.states.set(State::Fullscreen);
            });

            // Resize windows and change view.
            self.set_view(View::Fullscreen(window.clone()));
            self.resize_visible();
        }
    }

    /// Switch from fullscreen back to workspace view.
    ///
    /// If a surface is supplied, the view will not be changed unless the
    /// supplied surface matches the current fullscreen surface.
    pub fn unfullscreen<'a>(&mut self, surface: impl Into<Option<&'a ToplevelSurface>>) {
        let surface = surface.into();

        let window = match &self.view {
            View::Fullscreen(window) => window.borrow_mut(),
            _ => return,
        };

        if surface.is_some_and(|surface| surface == &window.surface) {
            // Update window's XDG state.
            window.surface.set_state(|state| {
                state.states.unset(State::Fullscreen);
            });
            drop(window);

            // Resize windows and change view.
            self.set_view(View::Workspace);
            self.resize_visible();
        }
    }

    /// Current window focus.
    pub fn focus(&mut self) -> Option<(WlSurface, Option<String>)> {
        // Always focus session lock surfaces.
        if let View::Lock(window) = &self.view {
            return window.as_ref().map(|window| (window.surface().clone(), window.app_id.clone()));
        }

        let focused = match self.layouts.focus.as_ref().map(Weak::upgrade) {
            // Use focused surface if the window is still alive.
            Some(Some(window)) => {
                // Clear urgency.
                let mut window = window.borrow_mut();
                window.urgent = false;

                Some((window.surface.clone(), window.app_id.clone()))
            },
            // Fallback to primary if secondary perished.
            Some(None) => {
                let active_layout = self.layouts.pending_active();
                let primary = active_layout.primary();
                let focused = primary.map(|window| {
                    // Clear urgency.
                    let mut window = window.borrow_mut();
                    window.urgent = false;

                    (window.surface.clone(), window.app_id.clone())
                });
                self.layouts.focus = primary.map(Rc::downgrade);
                focused
            },
            // Do not upgrade if toplevel is explicitly unfocused.
            None => None,
        };

        // Update window activation state.
        if self.activated.as_ref() != focused.as_ref().map(|(surface, _)| surface) {
            // Clear old activated flag.
            if let Some(activated) = self.activated.take() {
                activated.set_state(|state| {
                    state.states.unset(State::Activated);
                });
            }

            // Set new activated flag.
            if let Some((surface, _)) = &focused {
                surface.set_state(|state| {
                    state.states.set(State::Activated);
                });
            }
            self.activated = focused.as_ref().map(|(surface, _)| surface.clone());
        }

        focused.map(|(surface, app_id)| (surface.surface().clone(), app_id))
            // Check for layer-shell window focus.
            .or_else(|| self.layers.focus.clone())
    }

    /// Update the focused window.
    pub fn set_focus(
        &mut self,
        layout: Option<Weak<RefCell<Window>>>,
        layer: Option<WlSurface>,
        app_id: Option<String>,
    ) {
        self.layouts.focus = layout;
        self.layers.focus = layer.map(|l| (l, app_id));
    }

    /// Start a new transaction.
    fn start_transaction(&mut self) -> &mut Transaction {
        start_transaction();
        self.transaction.get_or_insert(Transaction::new())
    }

    /// Attempt to execute pending transactions.
    ///
    /// This will return the duration until the transaction should be timed out
    /// when there is an active transaction but it cannot be completed yet.
    pub fn update_transaction(&mut self) -> Option<Duration> {
        // Skip update if no transaction is active.
        let start = TRANSACTION_START.load(Ordering::Relaxed);
        if start == 0 {
            return None;
        }

        // Check if the transaction requires updating.
        let elapsed = UNIX_EPOCH.elapsed().unwrap().as_millis() as u64 - start;
        let timeout = self.transaction_timeout();
        if elapsed <= timeout {
            let lock_ready = match self.pending_view() {
                // Check if lock surface transaction is done.
                View::Lock(Some(window)) => window.transaction_done(),
                // Wait for timeout to allow surface creation.
                View::Lock(None) => false,
                _ => true,
            };

            // Check if all participants are ready.
            let finished = lock_ready
                && self.layouts.windows().all(|window| window.transaction_done())
                && self.layers.iter().all(Window::transaction_done);

            // Abort if the transaction is still pending.
            if !finished {
                let delta = timeout - elapsed;
                return Some(Duration::from_millis(delta));
            }
        }

        // Clear transaction timer.
        TRANSACTION_START.store(0, Ordering::Relaxed);

        // Store old visible window count to see if we need to redraw.
        let old_layout_count = self.layouts.active().window_count();
        let old_layer_count = self.layers.len();

        // Switch active view.
        if let Some(view) = self.transaction.take().and_then(|transaction| transaction.view) {
            self.dirty = true;
            self.view = view;
        }

        // Apply layout/liveliness changes.
        self.dirty |= self.layouts.apply_transaction(&self.output);

        // Update layer shell windows.
        self.layers.apply_transaction();

        // Update session lock window.
        if let View::Lock(Some(window)) = &mut self.view {
            window.apply_transaction();
        }

        // Update canvas and force redraw on orientation change.
        let old_canvas = mem::replace(&mut self.canvas, *self.output.canvas());
        self.dirty |= old_canvas.orientation() != self.canvas.orientation()
            || old_canvas.scale() != self.canvas.scale();

        // Close overview if all layouts died.
        if self.layouts.is_empty()
            && matches!(self.view, View::Overview(_) | View::DragAndDrop(_) | View::Fullscreen(_))
        {
            self.view = View::Workspace;
        }

        // Redraw if a visible window has died.
        self.dirty |= old_layout_count != self.layouts.active().window_count()
            || old_layer_count != self.layers.len();

        None
    }

    /// Get timeout before transactions will be forcefully applied.
    fn transaction_timeout(&self) -> u64 {
        match self.pending_view() {
            // Enforce a tighter transaction timing for session locking.
            View::Lock(_) => MAX_LOCKED_TRANSACTION_MILLIS,
            _ => MAX_TRANSACTION_MILLIS,
        }
    }

    /// Resize all windows to their expected size.
    pub fn resize_all(&mut self) {
        let available_fullscreen = self.output.available_fullscreen();
        let output_size = self.output.size();
        let scale = self.output.scale();

        // Handle fullscreen/lock surfaces.
        let fullscreen_window = match self.pending_view_mut() {
            View::Fullscreen(window) => {
                let mut window = window.borrow_mut();
                window.set_dimensions(scale, available_fullscreen);

                Some(window.surface().clone())
            },
            View::Lock(Some(window)) => {
                let rect = Rectangle::from_size(output_size);
                window.set_dimensions(scale, rect);

                None
            },
            _ => None,
        };
        let fullscreen_window = fullscreen_window.as_ref();

        // Resize XDG clients.
        for layout in self.layouts.layouts() {
            // Skip resizing fullscreened layout.
            if layout.windows().any(|window| Some(window.borrow().surface()) == fullscreen_window) {
                continue;
            }

            layout.resize(&self.output);
        }

        // Resize layer shell clients.
        for window in self.layers.iter_mut() {
            let fullscreened =
                window.surface.layer() == Layer::Overlay && fullscreen_window.is_some();
            window.update_dimensions(&mut self.output, fullscreened);
        }
    }

    /// Resize visible windows to their expected size.
    pub fn resize_visible(&mut self) {
        let available_fullscreen = self.output.available_fullscreen();
        let output_scale = self.output.scale();
        let output_size = self.output.size();

        match self.pending_view_mut() {
            // Resize fullscreen and overlay surfaces in fullscreen view.
            View::Fullscreen(window) => {
                // Resize fullscreen XDG client.
                window.borrow_mut().set_dimensions(output_scale, available_fullscreen);

                // Resize overlay layer clients.
                for window in self.layers.overlay_mut() {
                    window.update_dimensions(&mut self.output, true);
                }
            },
            View::Lock(Some(window)) => {
                let rect = Rectangle::from_size(output_size);
                window.set_dimensions(output_scale, rect);
            },
            // Resize active XDG layout and layer shell in any other view.
            _ => {
                // Resize XDG windows.
                self.layouts.pending_active().resize(&self.output);

                // Resize layer shell windows.
                for window in self.layers.iter_mut() {
                    window.update_dimensions(&mut self.output, false);
                }
            },
        }
    }

    /// Set output orientation, completely bypassing locking logic.
    fn set_orientation(&mut self, orientation: Orientation) {
        // Start transaction to ensure output transaction will be applied.
        start_transaction();

        // Update output orientation.
        self.output.set_orientation(orientation);

        // Update window transform.
        let transform = orientation.surface_transform();
        with_all_windows!(self, |window| window.update_transform(transform));

        // Resize all windows to new output size.
        self.resize_all();
    }

    /// Update output orientation.
    pub fn update_orientation(&mut self, orientation: Orientation) {
        self.unlocked_orientation = orientation;

        // Ignore orientation changes during orientation lock.
        if self.orientation_locked {
            return;
        }

        self.set_orientation(orientation);
    }

    /// Lock the output's orientation.
    pub fn lock_orientation(&mut self, orientation: Option<Orientation>) {
        // Change to the new locked orientation.
        if let Some(orientation) = orientation {
            self.set_orientation(orientation);
        }

        self.orientation_locked = true;
    }

    /// Unlock the output's orientation.
    pub fn unlock_orientation(&mut self) {
        self.orientation_locked = false;
        self.update_orientation(self.unlocked_orientation);
    }

    /// Get orientation lock state.
    pub fn orientation_locked(&self) -> bool {
        self.orientation_locked
    }

    /// Check if any window was damaged since the last redraw.
    pub fn damaged(&mut self) -> bool {
        if self.dirty {
            return true;
        }

        match &self.view {
            View::Overview(overview) if overview.dirty() => true,
            View::Workspace | View::Overview(_) => {
                self.layouts.windows().any(|window| window.dirty())
                    || self.layers.iter().any(Window::dirty)
            },
            View::Fullscreen(window) => {
                window.borrow().dirty() || self.layers.overlay().any(Window::dirty)
            },
            View::Lock(window) => window.as_ref().is_some_and(|window| window.dirty()),
            View::DragAndDrop(_) => false,
        }
    }

    /// Handle start of touch input.
    pub fn on_touch_start(&mut self, point: Point<f64, Logical>) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            _ => return,
        };

        // Hold on overview window stages it for D&D.
        if let Some(position) = overview.layout_position(&self.output, &self.layouts, point) {
            overview.start_hold(&self.event_loop, position);
        }

        overview.drag_action = Default::default();
        overview.last_drag_point = point;
        overview.y_offset = 0.;
    }

    /// Hand quick touch input.
    pub fn on_tap(&mut self, point: Point<f64, Logical>, toggle_ime: &mut bool) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            // Handle IME override toggle on gesture handle tap.
            View::Workspace => {
                *toggle_ime = point.y >= (self.canvas.size().h - GESTURE_HANDLE_HEIGHT) as f64
                    && self.focus().is_none();
                return;
            },
            View::DragAndDrop(_) | View::Fullscreen(_) | View::Lock(_) => return,
        };

        overview.cancel_hold(&self.event_loop);

        // Click inside window opens it as new primary.
        if let Some(position) = overview.layout_position(&self.output, &self.layouts, point) {
            self.layouts.set_active(&self.output, Some(position), true);
        }

        // Return to workspace view.
        //
        // If the click was outside of the focused window, we just close out of the
        // Overview and return to the previous primary/secondary windows.
        self.set_view(View::Workspace);
    }

    /// Hand quick double-touch input.
    pub fn on_double_tap(&mut self, point: Point<f64, Logical>) {
        // Ensure we're in workspace view.
        if !matches!(self.view, View::Workspace) {
            return;
        }

        // Ignore tap outside of gesture handle.
        let canvas_size = self.canvas.size().to_f64();
        if point.y < (canvas_size.h - GESTURE_HANDLE_HEIGHT as f64) {
            return;
        }

        if point.x >= canvas_size.w / 1.5 {
            self.layouts.cycle_active(&self.output, 1);
        } else if point.x < canvas_size.w / 3. {
            self.layouts.cycle_active(&self.output, -1);
        }
    }

    /// Handle a touch drag.
    pub fn on_drag(&mut self, touch_state: &mut TouchState, mut point: Point<f64, Logical>) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            View::DragAndDrop(dnd) => {
                // Cancel velocity and clamp if touch position is outside the screen.
                let output_size = self.output.wm_size().to_f64();
                if point.x < 0.
                    || point.x > output_size.w
                    || point.y < 0.
                    || point.y > output_size.h
                {
                    point.x = point.x.clamp(0., output_size.w - 1.);
                    point.y = point.y.clamp(0., output_size.h - 1.);
                    touch_state.cancel_velocity();
                }

                let delta = point - mem::replace(&mut dnd.touch_position, point);
                dnd.window_position += delta;

                // Redraw when the D&D window is moved.
                self.dirty = true;

                return;
            },
            View::Fullscreen(_) | View::Lock(_) | View::Workspace => return,
        };

        let delta = point - mem::replace(&mut overview.last_drag_point, point);

        // Lock current drag direction if it hasn't been determined yet.
        if matches!(overview.drag_action.action_type, DragActionType::None) {
            if delta.x.abs() < delta.y.abs() {
                overview.drag_action = overview
                    .layout_position(&self.output, &self.layouts, point)
                    .and_then(|position| self.layouts.window_at(position))
                    .map(|window| DragActionType::Close(Rc::downgrade(window)).into())
                    .unwrap_or_default();
            } else {
                overview.drag_action = DragActionType::Cycle.into();
            }
        }

        // Update drag action.
        match overview.drag_action.action_type {
            DragActionType::Cycle => {
                let sensitivity = self.output.physical_size().w as f64 * 0.4;
                overview.x_offset += delta.x / sensitivity;
            },
            DragActionType::Close(_) => overview.y_offset += delta.y,
            DragActionType::None => (),
        }

        // Cancel velocity once drag actions are completed.
        if overview.cycle_edge_reached(self.layouts.len()) {
            touch_state.cancel_velocity();
        }

        overview.cancel_hold(&self.event_loop);

        // Redraw when cycling through the overview.
        self.dirty = true;
    }

    /// Handle touch drag release.
    pub fn on_drag_release(&mut self) {
        match &mut self.view {
            View::Overview(overview) => overview.drag_action.done = true,
            View::DragAndDrop(dnd) => {
                let (primary_bounds, secondary_bounds) = dnd.drop_bounds(&self.output);
                if primary_bounds.to_f64().contains(dnd.touch_position) {
                    let surface = dnd.window.borrow().surface().clone();
                    if let Some(position) = self.layouts.position(&surface) {
                        self.layouts.set_primary(&self.output, position);
                        self.set_view(View::Workspace);
                    }
                } else if secondary_bounds.to_f64().contains(dnd.touch_position) {
                    let surface = dnd.window.borrow().surface().clone();
                    if let Some(position) = self.layouts.position(&surface) {
                        self.layouts.set_secondary(&self.output, position);
                        self.set_view(View::Workspace);
                    }
                } else {
                    let overview = Overview::new(dnd.overview_x_offset, None);
                    self.set_view(View::Overview(overview));
                }
            },
            View::Fullscreen(_) | View::Lock(_) | View::Workspace => (),
        }
    }

    /// Process in-progress handle gestures.
    pub fn on_gesture(&mut self, touch_state: &mut TouchState, gesture: HandleGesture) {
        match (gesture, &mut self.view) {
            (HandleGesture::Vertical(position), View::Overview(overview)) => {
                overview.set_open_percentage(&self.output, position);

                // Ensure we don't keep processing velocity after completion.
                if overview.gesture_threshold_passed() && !touch_state.touching() {
                    touch_state.cancel_velocity();
                    self.on_gesture_done(gesture);
                }
            },
            (HandleGesture::Vertical(position), View::Workspace) if !self.layouts.is_empty() => {
                // Ignore overview gesture until changes are required.
                let available = self.output.available_overview().to_f64();
                if position < available.loc.y || position >= available.loc.y + available.size.h {
                    return;
                }

                // Change view and resize windows.
                let active_empty = self.layouts.active().is_empty();
                let primary_percentage = Some(if active_empty { 0. } else { 1. });
                let overview = Overview::new(self.layouts.active_offset(), primary_percentage);
                self.set_view(View::Overview(overview));
            },
            (HandleGesture::Vertical(_) | HandleGesture::Horizontal, _) => (),
        }
    }

    /// Process completion of handle gestures.
    pub fn on_gesture_done(&mut self, gesture: HandleGesture) {
        match (gesture, &mut self.view) {
            (HandleGesture::Vertical(position), View::Overview(overview)) if overview.opened => {
                overview.set_open_percentage(&self.output, position);

                if overview.gesture_threshold_passed() {
                    // Switch back to workspace view.
                    self.layouts.set_active(&self.output, None, true);
                    self.set_view(View::Workspace);
                } else {
                    // Stay in overview.
                    let overview = Overview::new(overview.x_offset, None);
                    self.set_view(View::Overview(overview));
                }
            },
            (HandleGesture::Vertical(position), View::Overview(overview)) if !overview.opened => {
                overview.set_open_percentage(&self.output, position);

                if overview.gesture_threshold_passed() {
                    overview.opened = true;
                } else {
                    self.set_view(View::Workspace);
                }
            },
            // Leave fullscreen on "drag up".
            (HandleGesture::Vertical(position), View::Fullscreen(window)) => {
                // Require drag end to be above the gesture handle.
                let available = self.output.available_overview().to_f64();
                if position < available.loc.y || position >= available.loc.y + available.size.h {
                    return;
                }

                // Unset XDG fullscreen state.
                window.borrow().surface.set_state(|state| {
                    state.states.unset(State::Fullscreen);
                });

                // Change back to workspace view.
                self.set_view(View::Workspace);

                // Resize back to workspace size.
                self.resize_visible();
            },
            (HandleGesture::Vertical(_) | HandleGesture::Horizontal, _) => (),
        }
    }

    /// Check which surface is at a specific touch point.
    ///
    /// This filters out non-interactive surfaces.
    pub fn surface_at(&mut self, position: Point<f64, Logical>) -> Option<InputSurface> {
        let scale = self.output.scale();

        /// Focus a layer shell surface and return it.
        macro_rules! focus_layer_surface {
            ($window:expr) => {{
                let mut surface = $window.surface_at(scale, position)?;

                // Only set new focus target if focus is accepted.
                if !$window.deny_focus {
                    let wl_surface = $window.surface().clone();
                    let app_id = $window.app_id.clone();
                    surface.toplevel = Some(InputSurfaceKind::Layer((wl_surface, app_id)));
                }

                Some(surface)
            }};
        }

        // Prevent window interaction in Overview/DnD.
        match &self.view {
            View::Workspace => (),
            View::Fullscreen(window) => {
                if let Some(window) = self.layers.overlay_window_at(scale, position) {
                    return focus_layer_surface!(window);
                }

                // Get surface of the fullscreened window.
                let window_ref = window.borrow();
                let mut surface = window_ref.surface_at(scale, position)?;

                // Set toplevel to update focus.
                let app_id = window_ref.app_id.clone();
                let window = Rc::downgrade(window);
                surface.toplevel = Some(InputSurfaceKind::Layout((window, app_id)));

                return Some(surface);
            },
            View::Lock(Some(window)) => return window.surface_at(scale, position),
            _ => return None,
        };

        // Search for topmost clicked surface.

        if let Some(window) = self.layers.foreground_window_at(scale, position) {
            return focus_layer_surface!(window);
        }

        if let Some(surface) = self.layouts.touch_surface_at(scale, position) {
            return Some(surface);
        }

        if let Some(window) = self.layers.background_window_at(scale, position) {
            return focus_layer_surface!(window);
        }

        None
    }

    /// Add a per-window scale override.
    pub fn add_window_scale(&mut self, app_id: AppIdMatcher, scale: WindowScale) {
        // Remove previous scale assignments for this EXACT regex.
        self.window_scales.retain(|(matcher, _)| matcher.base() != app_id.base());

        // Add scale if it is transformative to the output scale.
        match scale {
            WindowScale::Additive(scale) | WindowScale::Subtractive(scale) if scale == 0. => (),
            WindowScale::Multiplicative(scale) | WindowScale::Divisive(scale) if scale == 1. => (),
            _ => self.window_scales.push((app_id, scale)),
        }

        // Update existing window scales.
        let window_scales = mem::take(&mut self.window_scales);
        let output_scale = self.output.scale();
        with_all_windows_mut!(self, |window| window.update_scale(&window_scales, output_scale));
        self.window_scales = window_scales;
    }

    /// Check if a surface is currently visible.
    pub fn surface_visible(&self, surface: &WlSurface) -> bool {
        match self.pending_view() {
            View::Lock(Some(window)) if window.owns_surface(surface) => return true,
            _ => (),
        }

        let mut visible_xdg = false;
        self.layouts.with_visible(|window| visible_xdg |= window.owns_surface(surface));

        visible_xdg || self.layers.iter().any(|layer| layer.owns_surface(surface))
    }

    /// Application runtime.
    pub fn runtime(&self) -> u32 {
        self.start_time.elapsed().as_millis() as u32
    }

    /// Get transaction view, or current view if no transaction is active.
    fn pending_view(&self) -> &View {
        self.transaction.as_ref().and_then(|t| t.view.as_ref()).unwrap_or(&self.view)
    }

    /// Get transaction view, or current view if no transaction is active.
    fn pending_view_mut(&mut self) -> &mut View {
        self.transaction.as_mut().and_then(|t| t.view.as_mut()).unwrap_or(&mut self.view)
    }

    /// Change the active view.
    fn set_view(&mut self, view: View) {
        // SAFETY: Prevent accidental unlock. Use [`Self::unlock`] instead.
        if let View::Lock(_) = self.pending_view() {
            return;
        }

        match view {
            // Skip transaction when switching to overview.
            View::Overview(_) => match &mut self.transaction {
                // Clear pending view to go to overview.
                Some(transaction) => {
                    transaction.view = None;
                    self.view = view;
                },
                // Directly apply overview without pending transaction.
                None => {
                    self.dirty = true;
                    self.view = view;
                },
            },
            _ => self.start_transaction().view = Some(view),
        }
    }

    /// Get immutable reference to the current output.
    pub fn output(&self) -> &Output {
        &self.output
    }

    /// Update the window manager's current output.
    pub fn set_output(&mut self, output: Output) {
        self.canvas = *output.canvas();
        self.output = output;
    }

    /// Get access to the current canvas.
    ///
    /// This is different from [`Self::output`] by returning a cached output
    /// while transactions are processed.
    pub fn canvas(&self) -> &Canvas {
        &self.canvas
    }

    /// Update the output's scale.
    pub fn set_scale(&mut self, scale: f64) {
        self.start_transaction();
        self.output.set_scale(scale);

        // Update surface's preferred fractional and buffer scale.
        with_all_windows!(self, |window| window.set_preferred_scale(scale));

        self.resize_all();
    }

    /// Mark the entire screen as dirty.
    pub fn set_dirty(&mut self) {
        self.dirty = true;
    }

    /// Raise a surface's window to the foreground.
    pub fn raise(&mut self, surface: &WlSurface) {
        if let Some(layout_position) = self.layouts.position(surface) {
            self.layouts.set_active(&self.output, Some(layout_position), false);
        }
    }

    /// Mark a surface's window as urgent.
    pub fn set_urgent(&mut self, surface: &WlSurface, urgent: bool) {
        if let Some(window) = self.layouts.find_window(surface) {
            window.borrow_mut().urgent = urgent;
        }
    }

    /// Get parent geometry of the window owning a surface.
    pub fn parent_geometry(&self, surface: &WlSurface) -> Rectangle<i32, Logical> {
        with_all_windows!(self, |window| {
            if window.owns_surface(surface) {
                return window.bounds(self.output().scale());
            }
        });

        // Default to full output size.
        Rectangle::from_size(self.output().size())
    }

    /// IME force enable/disable status.
    ///
    /// This is only responsible for determining the gesture handle color, must
    /// only be used through [`Catacomb::toggle_ime_override`].
    pub fn set_ime_override(&mut self, ime_override: Option<bool>) {
        self.ime_override = ime_override;
        self.dirty = true;
    }
}

/// Atomic changes to [`Windows`].
#[derive(Debug)]
struct Transaction {
    view: Option<View>,
}

impl Transaction {
    fn new() -> Self {
        Self { view: None }
    }
}

/// Compositor window arrangements.
#[derive(Default, Debug)]
enum View {
    /// List of all open windows.
    Overview(Overview),
    /// Drag and drop for tiling windows.
    DragAndDrop(DragAndDrop),
    /// Fullscreened XDG-shell window.
    Fullscreen(Rc<RefCell<Window>>),
    /// Session lock.
    Lock(Option<Box<Window<LockSurface>>>),
    /// Currently active windows.
    #[default]
    Workspace,
}
