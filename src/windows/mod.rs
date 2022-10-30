//! Window management.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::mem;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

use smithay::backend::renderer::gles2::ffi::{self as gl, Gles2};
use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::Frame;
use smithay::reexports::calloop::LoopHandle;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::State;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{Logical, Physical, Point, Rectangle};
use smithay::wayland::compositor;
use smithay::wayland::shell::wlr_layer::Layer;
use smithay::wayland::shell::xdg::{PopupSurface, ToplevelSurface};

use crate::catacomb::{Catacomb, Damage};
use crate::drawing::{Graphics, MAX_DAMAGE_AGE};
use crate::input::{Gesture, TouchState};
use crate::layer::Layers;
use crate::orientation::Orientation;
use crate::output::{Output, GESTURE_HANDLE_HEIGHT};
use crate::overview::{Direction, DragAndDrop, Overview};
use crate::windows::surface::{CatacombLayerSurface, OffsetSurface, Surface};
use crate::windows::window::Window;

pub mod surface;
pub mod window;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_DURATION: Duration = Duration::from_secs(1);

/// Horizontal sensitivity of the application overview.
const OVERVIEW_HORIZONTAL_SENSITIVITY: f64 = 250.;

/// Relative size of gesture notch to the handle's whole width/height.
const GESTURE_NOTCH_PERCENTAGE: f64 = 0.2;

/// Gesture handle foreground color.
const GESTURE_HANDLE_NOTCH_COLOR: [f32; 4] = [1., 1., 1., 1.];

/// Gesture handle background color.
const GESTURE_HANDLE_COLOR: [f32; 4] = [0., 0., 0., 1.];

/// Container tracking all known clients.
#[derive(Debug)]
pub struct Windows {
    pub output: Output,

    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    view: View,

    windows: Vec<Rc<RefCell<Window>>>,
    orphan_popups: Vec<Window<PopupSurface>>,
    layers: Layers,

    event_loop: LoopHandle<'static, Catacomb>,
    transaction: Option<Transaction>,
    start_time: Instant,
    graphics: Graphics,
    focus: Focus,

    /// Orientation used for the window's current rendered state.
    ///
    /// This is used to keep rendering at the previous orientation when a device
    /// was rotated and there is an active transaction pending waiting for
    /// clients to submit new buffers.
    orientation: Orientation,
    /// Orientation independent from [`orientation_locked`] state.
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
            secondary: Default::default(),
            graphics: Default::default(),
            windows: Default::default(),
            primary: Default::default(),
            layers: Default::default(),
            focus: Default::default(),
            view: Default::default(),
        }
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface) {
        self.windows.push(Rc::new(RefCell::new(Window::new(surface))));
        self.set_primary(self.windows.len() - 1);
        self.set_secondary(None);
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

        self.windows
            .iter_mut()
            .map(|window| window.borrow_mut())
            .find(|window| window.surface().eq(&wl_surface))
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
                $windows.find(|window| window.surface().eq(&root_surface))
            }};
        }

        // Handle XDG surface commits.
        let mut windows = self.windows.iter().map(|window| window.borrow_mut());
        if let Some(mut window) = find_window!(windows) {
            window.surface_commit_common(surface, &self.output);
            return;
        }

        // Handle popup orphan adoption.
        self.orphan_surface_commit(&root_surface);

        // Apply popup surface commits.
        for window in &mut self.windows {
            window.borrow_mut().popup_surface_commit(&root_surface, surface, &self.output);
        }

        // Handle layer shell surface commits.
        let old_exclusive = self.output.exclusive;
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        if let Some(window) = find_window!(self.layers.iter_mut()) {
            window.surface_commit(surface, &mut self.output, transaction);
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
        if let Some(primary) = self.primary.upgrade() {
            popup = primary.borrow_mut().add_popup(popup, &parent)?;
        }

        // Try and add it to the secondary window.
        if let Some(secondary) = self.secondary.upgrade() {
            popup = secondary.borrow_mut().add_popup(popup, &parent)?;
        }

        // Dismiss popup if it wasn't added to either of the visible windows.
        popup.surface.send_popup_done();

        Some(())
    }

    /// Draw the current window state.
    pub fn draw(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
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
            || self.view != View::Workspace
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

        self.layers.draw_background(renderer, frame, &self.output, damage);

        match self.view {
            View::Workspace => {
                self.with_visible(|window| {
                    window.draw(renderer, frame, &self.output, 1., None, damage)
                });
            },
            View::DragAndDrop(ref dnd) => {
                self.with_visible(|window| {
                    window.draw(renderer, frame, &self.output, 1., None, damage)
                });
                dnd.draw(renderer, frame, &self.output, &self.windows, &mut self.graphics);
            },
            View::Overview(ref mut overview) => {
                overview.draw(renderer, frame, &self.output, &self.windows);

                // Stage immediate redraw while overview animations are active.
                if overview.animating_drag(self.windows.len()) {
                    self.fully_damaged = true;
                }
            },
        }

        let workspace_active = self.view == View::Workspace;
        self.layers.draw_foreground(renderer, frame, &self.output, damage, workspace_active);

        // Draw gesture handle in workspace view.
        let _ = renderer.with_context(|_, gl| unsafe {
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
        if self.view == View::Workspace {
            let runtime = self.runtime();
            self.layers.request_frames(runtime);
            self.with_visible(|window| window.request_frame(runtime));
        }
    }

    /// Stage dead layer shell windows for reaping.
    pub fn refresh_layers(&mut self) {
        self.transaction.get_or_insert(Transaction::new(self));

        // Handle layer shell death.
        let old_exclusive = self.output.exclusive;
        for window in self.layers.iter().filter(|window| !window.alive()) {
            self.output.exclusive.reset(window.surface.anchor, window.surface.exclusive_zone);
        }

        // Resize windows if reserved layer space changed.
        if self.output.exclusive != old_exclusive {
            self.resize_all();
        }
    }

    /// Reap dead XDG popup windows.
    pub fn refresh_popups(&mut self) {
        for window in &mut self.windows {
            window.borrow_mut().refresh_popups();
        }
    }

    /// Stage dead visible windows for reaping.
    ///
    /// This will reorder and resize visible windows when any of them has died.
    pub fn refresh_visible(&mut self) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        let primary = transaction.primary.upgrade();
        let secondary = transaction.secondary.upgrade();

        // Remove dead primary/secondary windows.
        if secondary.as_ref().map_or(true, |window| !window.borrow().alive()) {
            transaction.secondary = Weak::new();
        }
        if primary.map_or(true, |window| !window.borrow().alive()) {
            transaction.primary = mem::take(&mut transaction.secondary);
        }

        transaction.update_visible_dimensions(&self.output);
    }

    /// Start Overview window Drag & Drop.
    pub fn start_dnd(&mut self) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            _ => return,
        };

        let index = overview.focused_index(self.windows.len());
        let dnd = DragAndDrop::new(overview.last_drag_point, overview.x_offset, index);
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
        let window = self.focus.toplevel.upgrade().or_else(|| self.primary.upgrade())?;
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

    /// Check if there's an active transaction.
    pub fn transaction_active(&self) -> bool {
        self.transaction.is_some()
    }

    /// Attempt to execute pending transactions.
    pub fn update_transaction(&mut self) {
        let transaction = match &mut self.transaction {
            Some(start) => start,
            None => return,
        };

        // Check if the transaction requires updating.
        if Instant::now().duration_since(transaction.start) <= MAX_TRANSACTION_DURATION {
            // Check if all participants are ready.
            let finished = self.windows.iter().all(|window| window.borrow().transaction_done())
                && self.layers.iter().all(|window| window.transaction_done());

            // Abort if the transaction is still pending.
            if !finished {
                return;
            }
        }

        let secondary_index = self.primary.strong_count().max(1);
        let mut i = self.windows.len();
        while i > 0 {
            i -= 1;

            // Remove dead windows.
            if !self.windows[i].borrow().alive() {
                self.windows.remove(i);
                continue;
            }

            // Apply transaction changes.
            self.windows[i].borrow_mut().apply_transaction();

            // Ensure primary/secondary are always first/second window.
            let weak = Rc::downgrade(&self.windows[i]);
            if i > 0 && transaction.primary.ptr_eq(&weak) {
                self.windows.swap(0, i);
                i += 1;
            } else if i > secondary_index && transaction.secondary.ptr_eq(&weak) {
                self.windows.swap(secondary_index, i);
                i += 1;
            }
        }

        // Update layer shell windows.
        self.layers.apply_transaction();

        // Apply window management changes.
        let transaction = self.transaction.take().unwrap();
        self.view = transaction.view.unwrap_or(self.view);
        self.orientation = transaction.orientation;
        self.secondary = transaction.secondary;
        self.primary = transaction.primary;
        self.fully_damaged = true;
    }

    /// Resize all windows to their expected size.
    pub fn resize_all(&mut self) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));

        // Resize invisible windows.
        for window in &self.windows {
            let mut window = window.borrow_mut();
            let rectangle = Rectangle::from_loc_and_size((0, 0), self.output.available().size);
            window.set_dimensions(transaction, rectangle);
        }

        // Resize primary/secondary.
        transaction.update_visible_dimensions(&self.output);

        // Resize layer shell windows.
        for window in self.layers.iter_mut() {
            window.update_dimensions(&mut self.output, transaction);
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

        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        transaction.orientation = orientation;

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
            || (self.view == View::Workspace
                && (self.primary.upgrade().map_or(false, |window| window.borrow().damaged())
                    || self.secondary.upgrade().map_or(false, |window| window.borrow().damaged())
                    || self.layers.iter().any(|window| window.damaged())))
    }

    /// Window damage since last redraw.
    ///
    /// This function collects the damage for every window, without taking
    /// global damage into account. To avoid unnecessary work,
    /// [`Windows::fully_damaged`] should be called first.
    fn window_damage(&self, damage: &mut Damage) {
        let primary_damage = self.primary.upgrade().and_then(|window| window.borrow().damage());
        let secondary_damage = self.secondary.upgrade().and_then(|window| window.borrow().damage());
        let layer_damage = self.layers.iter().filter_map(|window| window.damage());

        for window_damage in layer_damage.chain(primary_damage).chain(secondary_damage) {
            damage.push(window_damage);
        }
    }

    /// Handle start of touch input.
    pub fn on_touch_start(&mut self, point: Point<f64, Logical>) {
        if let View::Overview(overview) = &mut self.view {
            // Click inside focused window stages it for opening as secondary.
            let window_bounds = overview.focused_bounds(&self.output);
            if window_bounds.contains(point.to_i32_round()) {
                overview.start_hold(&self.event_loop);
            }

            overview.last_drag_point = point;
            overview.drag_direction = None;
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

        // Click inside focused window opens it as primary.
        let window_bounds = overview.focused_bounds(&self.output);
        if !self.windows.is_empty() && window_bounds.contains(point.to_i32_round()) {
            let index = overview.focused_index(self.windows.len());

            // Clear secondary unless *only* primary is empty.
            self.set_primary(index);
            if self.primary.strong_count() > 0 {
                self.set_secondary(None);
            }
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

        let drag_direction = overview.drag_direction.get_or_insert_with(|| {
            if delta.x.abs() >= delta.y.abs() {
                Direction::Horizontal
            } else {
                Direction::Vertical
            }
        });

        match drag_direction {
            Direction::Horizontal => overview.x_offset += delta.x / OVERVIEW_HORIZONTAL_SENSITIVITY,
            Direction::Vertical => overview.y_offset += delta.y,
        }

        // Cancel velocity once drag actions are completed.
        if overview.should_close(&self.output) || overview.overdrag_limited(self.windows.len()) {
            touch_state.cancel_velocity();
        }

        overview.last_overdrag_step = None;
        overview.cancel_hold(&self.event_loop);

        // Redraw when cycling through the overview.
        self.fully_damaged = true;
    }

    /// Handle touch drag release.
    pub fn on_drag_release(&mut self) {
        match self.view {
            View::Overview(ref mut overview) => {
                let should_close = overview.should_close(&self.output);

                overview.last_overdrag_step = Some(Instant::now());
                overview.y_offset = 0.;

                // Close window if y offset exceeds the threshold.
                if should_close && !self.windows.is_empty() {
                    let index = overview.focused_index(self.windows.len());
                    self.windows[index].borrow_mut().surface.send_close();
                    self.windows.remove(index);
                    self.refresh_visible();

                    // Close overview after all windows were closed.
                    if self.windows.is_empty() {
                        self.set_view(View::Workspace);
                    }
                }
            },
            View::DragAndDrop(dnd) => {
                let (primary_bounds, secondary_bounds) = dnd.drop_bounds(&self.output);
                if primary_bounds.to_f64().contains(dnd.touch_position) {
                    self.set_primary(dnd.window_index);
                    self.set_view(View::Workspace);
                } else if secondary_bounds.to_f64().contains(dnd.touch_position) {
                    self.set_secondary(dnd.window_index);
                    self.set_view(View::Workspace);
                } else {
                    let overview = Overview { x_offset: dnd.overview_x_offset, ..Overview::new() };
                    self.set_view(View::Overview(overview));
                }
            },
            View::Workspace => (),
        }
    }

    /// Handle touch gestures.
    pub fn on_gesture(&mut self, gesture: Gesture) {
        match (gesture, self.view) {
            (Gesture::DragUp, View::Overview(_)) => {
                self.focus.toplevel = Weak::new();
                self.set_secondary(None);
                self.set_primary(None);
                self.set_view(View::Workspace);
            },
            (Gesture::DragUp, _) if !self.windows.is_empty() => {
                self.set_view(View::Overview(Overview::default()));
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

        for window in self.primary.upgrade().iter().chain(&self.secondary.upgrade()) {
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
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        transaction.view = Some(view);
    }

    /// Execute a function for all visible windows.
    fn with_visible<F: FnMut(&mut Window)>(&self, mut fun: F) {
        for window in self.primary.upgrade().iter_mut().chain(&mut self.secondary.upgrade()) {
            fun(&mut window.borrow_mut());
        }
    }

    /// Change the primary window.
    fn set_primary(&mut self, index: impl Into<Option<usize>>) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        let window = index.into().map(|index| &self.windows[index]);

        // Ignore no-ops.
        let weak_window =
            window.map(Rc::downgrade).unwrap_or_else(|| mem::take(&mut transaction.secondary));
        if weak_window.ptr_eq(&transaction.primary) {
            return;
        }

        // Update output's visible windows.
        if let Some(primary) = transaction.primary.upgrade() {
            primary.borrow_mut().leave(transaction, &self.output);
        }
        if let Some(window) = &window {
            self.focus.toplevel = Rc::downgrade(window);
            window.borrow_mut().enter(&self.output);
        }

        // Clear secondary if it's the new primary.
        if weak_window.ptr_eq(&transaction.secondary) {
            transaction.secondary = Weak::new();
        }

        // Set primary and move old one to secondary if it is empty.
        let old_primary = mem::replace(&mut transaction.primary, weak_window);
        if transaction.secondary.strong_count() == 0 && transaction.primary.strong_count() != 0 {
            transaction.secondary = old_primary;
        }

        transaction.update_visible_dimensions(&self.output);
    }

    /// Change the secondary window.
    fn set_secondary(&mut self, index: impl Into<Option<usize>>) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        let window = index.into().map(|i| &self.windows[i]);

        // Ignore no-ops.
        let weak_window = window.map(Rc::downgrade).unwrap_or_default();
        if weak_window.ptr_eq(&transaction.secondary) {
            return;
        }

        // Update output's visible windows.
        if let Some(secondary) = transaction.secondary.upgrade() {
            secondary.borrow_mut().leave(transaction, &self.output);
        }
        if let Some(window) = &window {
            self.focus.toplevel = Rc::downgrade(window);
            window.borrow_mut().enter(&self.output);
        }

        // Clear primary if it's the new secondary.
        if weak_window.ptr_eq(&transaction.primary) {
            transaction.primary = Weak::new();
        }

        transaction.secondary = weak_window;
        transaction.update_visible_dimensions(&self.output);
    }
}

/// Atomic changes to [`Windows`].
#[derive(Clone, Debug)]
pub struct Transaction {
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    orientation: Orientation,
    view: Option<View>,
    start: Instant,
}

impl Transaction {
    fn new(current_state: &Windows) -> Self {
        Self {
            primary: current_state.primary.clone(),
            secondary: current_state.secondary.clone(),
            orientation: current_state.orientation,
            start: Instant::now(),
            view: None,
        }
    }

    /// Update visible window dimensions.
    pub fn update_visible_dimensions(&mut self, output: &Output) {
        if let Some(mut primary) = self.primary.upgrade().as_ref().map(|s| s.borrow_mut()) {
            let secondary_visible = self.secondary.strong_count() > 0;
            let rectangle = output.primary_rectangle(secondary_visible);
            primary.set_dimensions(self, rectangle);
        }

        if let Some(mut secondary) = self.secondary.upgrade().as_ref().map(|s| s.borrow_mut()) {
            let rectangle = output.secondary_rectangle();
            secondary.set_dimensions(self, rectangle);
        }
    }
}

/// Compositor window arrangements.
#[derive(Copy, Clone, PartialEq, Debug)]
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
