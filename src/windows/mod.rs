//! Window management.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::mem;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Instant, UNIX_EPOCH};

use smithay::backend::renderer::gles2::ffi::{self as gl, Gles2};
use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::Frame;
use smithay::reexports::calloop::LoopHandle;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::State;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{Logical, Physical, Point, Rectangle};
use smithay::wayland::compositor;
use smithay::wayland::shell::wlr_layer::{Layer, LayerSurface};
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface};

use crate::catacomb::{Catacomb, Damage};
use crate::drawing::{Graphics, MAX_DAMAGE_AGE};
use crate::input::{Gesture, TouchState};
use crate::layer::Layers;
use crate::orientation::Orientation;
use crate::output::{Output, GESTURE_HANDLE_HEIGHT};
use crate::overview::{DragAction, DragAndDrop, Overview};
use crate::windows::layout::{LayoutPosition, Layouts};
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};
use crate::windows::window::Window;

pub mod layout;
pub mod surface;
pub mod window;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_SECS: u64 = 1;

/// Horizontal sensitivity of the application overview.
const OVERVIEW_HORIZONTAL_SENSITIVITY: f64 = 250.;

/// Relative size of gesture notch to the handle's whole width/height.
const GESTURE_NOTCH_PERCENTAGE: f64 = 0.2;

/// Gesture handle foreground color.
const GESTURE_HANDLE_NOTCH_COLOR: [f32; 4] = [1., 1., 1., 1.];

/// Gesture handle background color.
const GESTURE_HANDLE_COLOR: [f32; 4] = [0., 0., 0., 1.];

/// Global transaction timer.
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

    let now = UNIX_EPOCH.elapsed().unwrap().as_secs();
    TRANSACTION_START.store(now, Ordering::Relaxed);
}

/// Container tracking all known clients.
#[derive(Debug)]
pub struct Windows {
    pub output: Output,

    layouts: Layouts,
    view: View,

    orphan_popups: Vec<Window<PopupSurface>>,
    layers: Layers,

    event_loop: LoopHandle<'static, Catacomb>,
    transaction: Option<Transaction>,
    start_time: Instant,
    focus: Focus,

    /// Orientation used for the window's current rendered state.
    ///
    /// This is used to keep rendering at the previous orientation when a device
    /// was rotated and there is an active transaction pending waiting for
    /// clients to submit new buffers.
    orientation: Orientation,
    /// Orientation independent from [`Windows::orientation_locked`] state.
    unlocked_orientation: Orientation,
    orientation_locked: bool,

    /// Compositor damage beyond window-internal changes.
    fully_damaged: bool,
}

impl Windows {
    pub fn new(display: &DisplayHandle, event_loop: LoopHandle<'static, Catacomb>) -> Self {
        Self {
            event_loop,
            output: Output::new_dummy(display),
            start_time: Instant::now(),
            fully_damaged: true,
            unlocked_orientation: Default::default(),
            orientation_locked: Default::default(),
            orphan_popups: Default::default(),
            transaction: Default::default(),
            orientation: Default::default(),
            layouts: Default::default(),
            layers: Default::default(),
            focus: Default::default(),
            view: Default::default(),
        }
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        self.layouts.create(&self.output, window);
    }

    /// Add a new layer shell window.
    pub fn add_layer(&mut self, layer: Layer, surface: impl Into<CatacombLayerSurface>) {
        let mut window = Window::new(surface.into());
        window.enter(&self.output);
        self.layers.add(layer, window);
    }

    /// Add a new popup window.
    pub fn add_popup(&mut self, popup: PopupSurface) {
        self.orphan_popups.push(Window::new(popup));
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

        // Handle XDG surface commits.
        if let Some(mut window) = find_window!(self.layouts.windows_mut()) {
            window.surface_commit_common(surface, &self.output);
            return;
        }

        // Handle popup orphan adoption.
        self.orphan_surface_commit(&root_surface);

        // Apply popup surface commits.
        for mut window in self.layouts.windows_mut() {
            window.popup_surface_commit(&root_surface, surface, &self.output);
        }

        // Handle layer shell surface commits.
        let old_exclusive = self.output.exclusive;
        if let Some(window) = find_window!(self.layers.iter_mut()) {
            window.surface_commit(surface, &mut self.output);
        }

        // Resize windows after exclusive zone change.
        if self.output.exclusive != old_exclusive {
            self.resize_all();
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
    pub fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        for mut window in self.layouts.windows_mut() {
            window.import_buffers(renderer);
        }

        for window in self.layers.iter_mut() {
            window.import_buffers(renderer);
        }
    }

    /// Draw the current window state.
    pub fn draw(
        &mut self,
        frame: &mut Gles2Frame,
        graphics: &Graphics,
        damage: &mut Damage,
        buffer_age: u8,
    ) {
        // Reset global damage.
        let fully_damaged = mem::take(&mut self.fully_damaged);

        // Collect pending damage.
        let max_age = MAX_DAMAGE_AGE as u8;
        let damage = if buffer_age == 0
            || buffer_age > max_age
            || fully_damaged
            || !matches!(self.view, View::Workspace)
        {
            let output_size = self.output.size().to_physical(self.output.scale());
            damage.push(Rectangle::from_loc_and_size((0, 0), output_size));
            damage.take_since(1)
        } else {
            self.window_damage(damage);
            damage.take_since(buffer_age)
        };

        // Clear the screen.
        let _ = frame.clear([1., 0., 1., 1.], damage);

        self.layers.draw_background(frame, &self.output, damage);

        match self.view {
            View::Workspace => {
                self.layouts.with_visible(|window| {
                    window.draw(frame, &self.output, 1., None, damage);
                });
            },
            View::DragAndDrop(ref dnd) => {
                self.layouts.with_visible(|window| {
                    window.draw(frame, &self.output, 1., None, damage);
                });
                dnd.draw(frame, &self.output, graphics);
            },
            View::Overview(ref mut overview) => {
                overview.draw(frame, &self.output, &self.layouts);

                // Stage immediate redraw while overview animations are active.
                if overview.animating_drag(self.layouts.len()) {
                    self.fully_damaged = true;
                }
            },
        }

        let workspace_active = matches!(self.view, View::Workspace);
        self.layers.draw_foreground(frame, &self.output, damage, workspace_active);

        // Draw gesture handle in workspace view.
        let _ = frame.with_context(|gl| unsafe {
            self.draw_gesture_handle(gl, damage);
        });
    }

    /// Draw the gesture handle.
    unsafe fn draw_gesture_handle(&self, gl: &Gles2, damage: &[Rectangle<i32, Physical>]) {
        let handle_height = GESTURE_HANDLE_HEIGHT * self.output.scale();
        let output_size = self.output.physical_resolution();

        // Calculate handle rectangle.
        let (handle_loc, handle_size) = match self.output.orientation() {
            Orientation::Portrait => {
                ((0, output_size.h - handle_height), (output_size.w, handle_height))
            },
            Orientation::InversePortrait => ((0, 0), (output_size.w, handle_height)),
            Orientation::Landscape => ((0, 0), (handle_height, output_size.h)),
            Orientation::InverseLandscape => {
                ((output_size.w - handle_height, 0), (handle_height, output_size.h))
            },
        };
        let handle_rect = Rectangle::from_loc_and_size(handle_loc, handle_size);

        // Skip rendering without damage.
        if damage.iter().all(|damage| !damage.overlaps(handle_rect)) {
            return;
        }

        gl.Enable(gl::SCISSOR_TEST);

        // Draw Background.
        gl.Scissor(handle_rect.loc.x, handle_rect.loc.y, handle_rect.size.w, handle_rect.size.h);
        gl.ClearColor(
            GESTURE_HANDLE_COLOR[0],
            GESTURE_HANDLE_COLOR[1],
            GESTURE_HANDLE_COLOR[2],
            GESTURE_HANDLE_COLOR[3],
        );
        gl.Clear(gl::COLOR_BUFFER_BIT);

        // Draw handle notch.
        let notch_height = (handle_rect.size.h as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;
        let notch_width = (handle_rect.size.w as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;
        let notch_x = handle_rect.loc.x + (handle_rect.size.w - notch_width) / 2;
        let notch_y = handle_rect.loc.y + (handle_rect.size.h - notch_height) / 2;
        gl.Scissor(notch_x, notch_y, notch_width, notch_height);
        gl.ClearColor(
            GESTURE_HANDLE_NOTCH_COLOR[0],
            GESTURE_HANDLE_NOTCH_COLOR[1],
            GESTURE_HANDLE_NOTCH_COLOR[2],
            GESTURE_HANDLE_NOTCH_COLOR[3],
        );
        gl.Clear(gl::COLOR_BUFFER_BIT);

        gl.Disable(gl::SCISSOR_TEST);
    }

    /// Request new frames for all visible windows.
    pub fn request_frames(&mut self) {
        let runtime = self.runtime();
        self.layers.request_frames(runtime);

        if matches!(self.view, View::Workspace) {
            self.layouts.with_visible(|window| window.request_frame(runtime));
        }
    }

    /// Stage dead XDG shell window for reaping.
    pub fn reap_xdg(&mut self, surface: &ToplevelSurface) {
        self.layouts.reap(&self.output, surface);
    }

    /// Stage dead layer shell window for reaping.
    pub fn reap_layer(&mut self, surface: LayerSurface) {
        // Handle layer shell death.
        let old_exclusive = self.output.exclusive;
        if let Some(window) = self.layers.iter().find(|layer| layer.surface.eq(&surface)) {
            self.output.exclusive.reset(window.surface.anchor, window.surface.exclusive_zone);
        }

        // Resize windows if reserved layer space changed.
        if self.output.exclusive != old_exclusive {
            self.resize_all();
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
        let window = match self.layouts.window(layout_position) {
            Some(window) => window.clone(),
            None => return,
        };

        let dnd = DragAndDrop::new(&self.output, overview, layout_position, window);
        self.view = View::DragAndDrop(dnd);
        self.fully_damaged = true;
    }

    /// Current window focus.
    pub fn focus(&mut self) -> Option<WlSurface> {
        // Check for layer-shell window focus.
        if let Some(surface) = &self.focus.layer {
            return Some(surface.clone());
        }

        // Check for toplevel window focus.
        let toplevel = self.focus.toplevel.upgrade();
        let window = toplevel.as_ref().or_else(|| self.layouts.active().primary())?;
        let surface = window.borrow().surface.clone();

        // Update window activation state.
        if self.focus.activated.as_ref() != Some(&surface) {
            // Clear old activated flag.
            let last_activated = self.focus.activated.replace(surface.clone());
            if let Some(activated) = last_activated {
                activated.set_state(|state| {
                    state.states.unset(State::Activated);
                });
            }

            // Set new activated flag.
            surface.set_state(|state| {
                state.states.set(State::Activated);
            });
        }

        Some(surface.surface().clone())
    }

    /// Start a new transaction.
    fn start_transaction(&mut self) -> &mut Transaction {
        start_transaction();
        self.transaction.get_or_insert(Transaction::new(self))
    }

    /// Check if there's an active transaction.
    pub fn transaction_active(&self) -> bool {
        TRANSACTION_START.load(Ordering::Relaxed) != 0
    }

    /// Attempt to execute pending transactions.
    pub fn update_transaction(&mut self) {
        // Skip update if no transaction is active.
        let start = TRANSACTION_START.load(Ordering::Relaxed);
        if start == 0 {
            return;
        }

        // Check if the transaction requires updating.
        let now = UNIX_EPOCH.elapsed().unwrap().as_secs();
        if now - start <= MAX_TRANSACTION_SECS {
            // Check if all participants are ready.
            let finished = self.layouts.windows().all(|window| window.transaction_done())
                && self.layers.iter().all(|window| window.transaction_done());

            // Abort if the transaction is still pending.
            if !finished {
                return;
            }
        }

        // Clear transaction timer.
        TRANSACTION_START.store(0, Ordering::Relaxed);

        // Apply layout/liveliness changes.
        self.layouts.apply_transaction(&self.output);

        // Update layer shell windows.
        self.layers.apply_transaction();

        // Apply window management changes.
        if let Some(transaction) = self.transaction.take() {
            self.orientation = transaction.orientation;
            if let Some(view) = transaction.view {
                self.view = view;
            }
        }

        // Close overview if all layouts died.
        if self.layouts.is_empty() {
            self.view = View::Workspace;
        }

        self.fully_damaged = true;
    }

    /// Resize all windows to their expected size.
    pub fn resize_all(&mut self) {
        // Resize XDG windows.
        self.layouts.resize_all(&self.output);

        // Resize layer shell windows.
        for window in self.layers.iter_mut() {
            window.update_dimensions(&mut self.output);
        }
    }

    /// Update output orientation.
    pub fn update_orientation(&mut self, orientation: Orientation) {
        self.unlocked_orientation = orientation;

        // Ignore orientation changes during orientation lock.
        if self.orientation_locked {
            return;
        }

        self.output.set_orientation(orientation);

        self.start_transaction().orientation = orientation;

        self.resize_all();
    }

    /// Lock the output's orientation.
    pub fn lock_orientation(&mut self, orientation: Option<Orientation>) {
        // Change to the new locked orientation.
        if let Some(orientation) = orientation {
            self.update_orientation(orientation);
        }

        self.orientation_locked = true;
    }

    /// Unlock the output's orientation.
    pub fn unlock_orientation(&mut self) {
        self.orientation_locked = false;
        self.update_orientation(self.unlocked_orientation);
    }

    /// Get the current rendering orientation.
    pub fn orientation(&self) -> Orientation {
        self.orientation
    }

    /// Check if any window was damaged since the last redraw.
    pub fn damaged(&mut self) -> bool {
        self.fully_damaged
            || self.layers.iter().any(|window| window.damaged())
            || self.layouts.windows().any(|window| window.damaged())
    }

    /// Window damage since last redraw.
    ///
    /// This function collects the damage for every window, without taking
    /// global damage into account. To avoid unnecessary work,
    /// [`Windows::fully_damaged`] should be called first.
    fn window_damage(&self, damage: &mut Damage) {
        let active_layout = self.layouts.active();
        let primary = active_layout.primary();
        let secondary = active_layout.secondary();

        let primary_damage = primary.and_then(|window| window.borrow().damage());
        let secondary_damage = secondary.and_then(|window| window.borrow().damage());
        let layer_damage = self.layers.iter().filter_map(|window| window.damage());

        for window_damage in layer_damage.chain(primary_damage).chain(secondary_damage) {
            damage.push(window_damage);
        }
    }

    /// Handle start of touch input.
    pub fn on_touch_start(&mut self, point: Point<f64, Logical>) {
        if let View::Overview(overview) = &mut self.view {
            // Hold on overview window stages it for D&D.
            if let Some(position) = overview.layout_position(&self.output, &self.layouts, point) {
                overview.start_hold(&self.event_loop, position);
            }

            overview.drag_action = DragAction::None;
            overview.last_drag_point = point;
            overview.y_offset = 0.;
        }
    }

    /// Hand quick touch input.
    pub fn on_tap(&mut self, point: Point<f64, Logical>) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            View::DragAndDrop(_) | View::Workspace => return,
        };

        overview.cancel_hold(&self.event_loop);

        // Click inside window opens it as new primary.
        if let Some(position) = overview.layout_position(&self.output, &self.layouts, point) {
            self.layouts.set_active(&self.output, Some(position.index));
        }

        // Return to workspace view.
        //
        // If the click was outside of the focused window, we just close out of the
        // Overview and return to the previous primary/secondary windows.
        self.set_view(View::Workspace);
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
                self.fully_damaged = true;

                return;
            },
            _ => return,
        };

        let delta = point - mem::replace(&mut overview.last_drag_point, point);

        // Lock current drag direction if it hasn't been determined yet.
        if matches!(overview.drag_action, DragAction::None) {
            if delta.x.abs() < delta.y.abs() {
                overview.drag_action = overview
                    .layout_position(&self.output, &self.layouts, point)
                    .and_then(|position| self.layouts.window(position))
                    .map(|window| DragAction::Close(Rc::downgrade(window)))
                    .unwrap_or_default();
            } else {
                overview.drag_action = DragAction::Cycle;
            }
        }

        // Update drag action.
        match overview.drag_action {
            DragAction::Cycle => overview.x_offset += delta.x / OVERVIEW_HORIZONTAL_SENSITIVITY,
            DragAction::Close(_) => overview.y_offset += delta.y,
            DragAction::None => (),
        }

        // Cancel velocity once drag actions are completed.
        if overview.should_close(&self.output) || overview.overdrag_limited(self.layouts.len()) {
            touch_state.cancel_velocity();
        }

        overview.last_overdrag_step = None;
        overview.cancel_hold(&self.event_loop);

        // Redraw when cycling through the overview.
        self.fully_damaged = true;
    }

    /// Handle touch drag release.
    pub fn on_drag_release(&mut self) {
        match &mut self.view {
            View::Overview(overview) => {
                let should_close = overview.should_close(&self.output);

                overview.last_overdrag_step = Some(Instant::now());
                overview.y_offset = 0.;

                // Close window if y offset exceeds the threshold.
                let closing_window = overview.drag_action.closing_window().upgrade();
                if let Some(closing_window) = closing_window.filter(|_| should_close) {
                    let surface = {
                        let mut window = closing_window.borrow_mut();
                        window.kill();
                        window.surface.clone()
                    };
                    self.layouts.reap(&self.output, &surface);
                }
            },
            View::DragAndDrop(dnd) => {
                let (primary_bounds, secondary_bounds) = dnd.drop_bounds(&self.output);
                if primary_bounds.to_f64().contains(dnd.touch_position) {
                    if let Some(position) = self.layouts.position(&dnd.window) {
                        self.layouts.set_primary(&self.output, position);
                        self.set_view(View::Workspace);
                    }
                } else if secondary_bounds.to_f64().contains(dnd.touch_position) {
                    if let Some(position) = self.layouts.position(&dnd.window) {
                        self.layouts.set_secondary(&self.output, position);
                        self.set_view(View::Workspace);
                    }
                } else {
                    let overview = Overview::new(dnd.overview_x_offset);
                    self.set_view(View::Overview(overview));
                }
            },
            View::Workspace => (),
        }
    }

    /// Handle touch gestures.
    pub fn on_gesture(&mut self, gesture: Gesture) {
        match (gesture, &self.view) {
            (Gesture::DragUp, View::Overview(_)) => {
                self.layouts.set_active(&self.output, None);
                self.focus.toplevel = Weak::new();
                self.set_view(View::Workspace);
            },
            (Gesture::DragUp, _) if !self.layouts.is_empty() => {
                let overview = Overview::new(self.layouts.active_offset());
                self.set_view(View::Overview(overview));
            },
            (Gesture::DragUp, _) => (),
        }
    }

    /// Check which surface is at a specific touch point.
    ///
    /// If the window at the touch location accepts keyboard input, this
    /// function will also change focus to the root window associated with
    /// the touch surface.
    pub fn touch_surface_at(&mut self, position: Point<f64, Logical>) -> Option<OffsetSurface> {
        // Prevent window interaction in Overview/DnD.
        match self.view {
            View::Workspace => (),
            _ => return None,
        };

        // Search for topmost clicked surface.

        if let Some(window) = self.layers.foreground_window_at(position) {
            if !window.deny_focus {
                self.focus.layer = Some(window.surface().clone());
            }
            return window.surface_at(position);
        }

        let active_layout = self.layouts.active();
        for window in active_layout.primary().iter().chain(&active_layout.secondary()) {
            let window_ref = window.borrow();
            if window_ref.contains(position) {
                self.focus.toplevel = Rc::downgrade(window);
                return window_ref.surface_at(position);
            }
        }

        if let Some(window) = self.layers.background_window_at(position) {
            if !window.deny_focus {
                self.focus.layer = Some(window.surface().clone());
            }
            return window.surface_at(position);
        }

        // Unfocus when touching outside of window bounds.
        self.focus.clear();

        None
    }

    /// Application runtime.
    pub fn runtime(&self) -> u32 {
        self.start_time.elapsed().as_millis() as u32
    }

    /// Change the active view.
    fn set_view(&mut self, view: View) {
        self.start_transaction().view = Some(view);
    }
}

/// Atomic changes to [`Windows`].
#[derive(Debug)]
struct Transaction {
    orientation: Orientation,
    view: Option<View>,
}

impl Transaction {
    fn new(current_state: &Windows) -> Self {
        Self { orientation: current_state.orientation, view: None }
    }
}

/// Compositor window arrangements.
#[derive(Debug)]
enum View {
    /// List of all open windows.
    Overview(Overview),
    /// Drag and drop for tiling windows.
    DragAndDrop(DragAndDrop),
    /// Currently active windows.
    Workspace,
}

impl Default for View {
    fn default() -> Self {
        View::Workspace
    }
}

/// Current window focus.
#[derive(Default, Debug)]
struct Focus {
    activated: Option<ToplevelSurface>,
    toplevel: Weak<RefCell<Window>>,
    layer: Option<WlSurface>,
}

impl Focus {
    /// Clear all window focus.
    fn clear(&mut self) {
        self.toplevel = Weak::new();
        self.layer = None;
    }
}
