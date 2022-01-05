//! Window management.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::cmp::{self, Ordering};
use std::mem;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

use smithay::backend::renderer::gles2::{ffi, Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::{self, BufferType, ImportAll};
use smithay::reexports::wayland_protocols::unstable::xdg_decoration;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::shell::xdg::{SurfaceCachedState, ToplevelSurface};
use wayland_protocols::xdg_shell::server::xdg_toplevel::State;
use xdg_decoration::v1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;

use crate::drawing::{Graphics, Texture};
use crate::geometry::CatacombVector;
use crate::input::HOLD_DURATION;
use crate::output::{Orientation, Output};
use crate::shell::SurfaceBuffer;

/// Percentage of output width reserved for the main window in the application overview.
pub const FG_OVERVIEW_PERCENTAGE: f64 = 0.75;

/// Percentage of remaining space reserved for background windows in the application overview.
const BG_OVERVIEW_PERCENTAGE: f64 = 0.5;

/// Horizontal sensitivity of the application overview.
const OVERVIEW_HORIZONTAL_SENSITIVITY: f64 = 250.;

/// Percentage of the output height a window can be moved before closing it in the overview.
const OVERVIEW_CLOSE_DISTANCE: f64 = 0.5;

/// Percentage of the screen for the drop highlight areas.
const DRAG_AND_DROP_PERCENTAGE: f64 = 0.3;

/// Animation speed for the return from close, lower means faster.
const CLOSE_CANCEL_ANIMATION_SPEED: f64 = 0.3;

/// Animation speed for the return from overdrag, lower means faster.
const OVERDRAG_ANIMATION_SPEED: f64 = 25.;

/// Maximum amount of overdrag before inputs are ignored.
const OVERDRAG_LIMIT: f64 = 3.;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_DURATION: Duration = Duration::from_millis(200);

/// Container tracking all known clients.
#[derive(Debug)]
pub struct Windows {
    windows: Vec<Rc<RefCell<Window>>>,
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    transaction: Option<Transaction>,
    start_time: Instant,
    graphics: Graphics,
    view: View,
}

impl Windows {
    pub fn new(renderer: &mut Gles2Renderer, output: &Output) -> Self {
        Self {
            graphics: Graphics::new(renderer, output).expect("texture creation error"),
            start_time: Instant::now(),
            transaction: Default::default(),
            secondary: Default::default(),
            windows: Default::default(),
            primary: Default::default(),
            view: Default::default(),
        }
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface, output: &Output) {
        self.windows.push(Rc::new(RefCell::new(Window::new(surface))));
        self.set_primary(output, self.windows.len() - 1);
        self.set_secondary(output, None);
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
    pub fn with_visible<F: FnMut(&mut Window)>(&self, mut fun: F) {
        for window in self.primary.upgrade().iter_mut().chain(&mut self.secondary.upgrade()) {
            fun(&mut window.borrow_mut());
        }
    }

    /// Draw the current window state.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output: &Output) {
        self.update_transaction();

        match self.view {
            View::Workspace => {
                self.with_visible(|window| window.draw(renderer, frame, output, 1., None));
            },
            View::DragAndDrop(ref dnd) => {
                self.with_visible(|window| window.draw(renderer, frame, output, 1., None));
                dnd.draw(renderer, frame, output, &self.windows, &mut self.graphics);
            },
            View::Overview(ref mut overview) => {
                overview.draw(renderer, frame, output, &self.windows, &mut self.graphics);
            },
        }
    }

    /// Request new frames for all visible windows.
    pub fn request_frames(&mut self) {
        if self.view == View::Workspace {
            let runtime = self.runtime();
            self.with_visible(|window| window.request_frame(runtime));
        }
    }

    /// Update window manager state.
    pub fn refresh(&mut self, output: &Output) {
        if self.windows.iter().any(|window| !window.borrow().surface.alive()) {
            self.refresh_visible(output);
        }

        // Start D&D on long touch in overview.
        if let View::Overview(overview) = &mut self.view {
            if overview.hold_start.map_or(false, |start| start.elapsed() >= HOLD_DURATION) {
                let index = overview.focused_index(self.windows.len());
                let dnd = DragAndDrop::new(overview.last_drag_point, overview.x_offset, index);
                self.view = View::DragAndDrop(dnd);
            }
        }
    }

    /// Reap dead visible windows.
    ///
    /// This will reorder and resize visible windows when any of them has died.
    fn refresh_visible(&mut self, output: &Output) {
        let transaction = self.start_transaction();

        // Remove dead primary/secondary windows.
        if transaction.secondary.upgrade().map_or(true, |window| !window.borrow().surface.alive()) {
            transaction.secondary = Weak::new();
        }
        if transaction.primary.upgrade().map_or(true, |window| !window.borrow().surface.alive()) {
            transaction.primary = mem::take(&mut transaction.secondary);
        }

        transaction.update_dimensions(output);
    }

    /// Create a new transaction, or access the active one.
    fn start_transaction(&mut self) -> &mut Transaction {
        self.transaction.get_or_insert(Transaction::new(self))
    }

    /// Attempt to execute pending transactions.
    fn update_transaction(&mut self) {
        let transaction = match &mut self.transaction {
            Some(start) => start,
            None => return,
        };

        // Check if the transaction requires updating.
        if Instant::now().duration_since(transaction.start) <= MAX_TRANSACTION_DURATION {
            // Check if all participants are ready.
            let finished = self.windows.iter().map(|window| window.borrow()).all(|window| {
                window
                    .transaction
                    .as_ref()
                    .map_or(true, |transaction| window.acked_size == transaction.rectangle.size)
            });

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
            if !self.windows[i].borrow().surface.alive() {
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

        // Apply window management changes.
        let transaction = self.transaction.take().unwrap();
        self.view = transaction.view.unwrap_or(self.view);
        self.secondary = transaction.secondary;
        self.primary = transaction.primary;
    }

    /// Resize all windows to their expected size.
    pub fn resize_all(&mut self, output: &Output) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));

        // Resize invisible windows.
        for window in &self.windows {
            let mut window = window.borrow_mut();
            let rectangle = Rectangle::from_loc_and_size((0, 0), output.size());
            window.update_dimensions(transaction, rectangle);
        }

        // Resize primary/secondary.
        transaction.update_dimensions(output);
    }

    /// Handle start of touch input.
    pub fn on_touch_start(&mut self, output: &Output, point: Point<f64, Logical>) {
        if let View::Overview(overview) = &mut self.view {
            // Click inside focused window stages it for opening as secondary.
            let window_bounds = overview.focused_bounds(output.size(), self.windows.len());
            if window_bounds.contains(point.to_i32_round()) {
                overview.hold_start = Some(Instant::now());
            }

            overview.last_drag_point = point;
            overview.drag_direction = None;
        }
    }

    /// Hand quick touch input.
    pub fn on_tap(&mut self, output: &Output, point: Point<f64, Logical>) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            View::DragAndDrop(_) | View::Workspace => return,
        };

        overview.hold_start = None;

        // Click inside focused window opens it as primary.
        let window_bounds = overview.focused_bounds(output.size(), self.windows.len());
        if window_bounds.contains(point.to_i32_round()) {
            let index = overview.focused_index(self.windows.len());

            // Clear secondary unless *only* primary is empty.
            self.set_primary(output, index);
            if self.primary.strong_count() > 0 {
                self.set_secondary(output, None);
            }

            self.set_view(View::Workspace);
        }
    }

    /// Handle a touch drag.
    pub fn on_drag(&mut self, point: Point<f64, Logical>) {
        let overview = match &mut self.view {
            View::Overview(overview) => overview,
            View::DragAndDrop(dnd) => {
                let delta = point - mem::replace(&mut dnd.touch_position, point);
                dnd.window_position += delta;
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

        overview.last_overdrag_step = None;
        overview.hold_start = None;
    }

    /// Handle touch release.
    pub fn on_drag_release(&mut self, output: &Output) {
        match self.view {
            View::Overview(ref mut overview) => {
                let close_distance = output.size().h as f64 * OVERVIEW_CLOSE_DISTANCE;
                let should_close = overview.y_offset.abs() >= close_distance;

                overview.last_overdrag_step = Some(Instant::now());
                overview.y_offset = 0.;

                // Close window if y offset exceeds the threshold.
                if should_close && !self.windows.is_empty() {
                    let index = overview.focused_index(self.windows.len());
                    self.windows[index].borrow_mut().surface.send_close();
                    self.windows.remove(index);
                    self.refresh_visible(output);
                }
            },
            View::DragAndDrop(dnd) => {
                let (primary_bounds, secondary_bounds) = dnd.drop_bounds(output);
                if primary_bounds.to_f64().contains(dnd.touch_position) {
                    self.set_primary(output, dnd.window_index);
                    self.set_view(View::Workspace);
                } else if secondary_bounds.to_f64().contains(dnd.touch_position) {
                    self.set_secondary(output, dnd.window_index);
                    self.set_view(View::Workspace);
                } else {
                    let overview = Overview { x_offset: dnd.overview_x_offset, ..Overview::new() };
                    self.set_view(View::Overview(overview));
                }
            },
            View::Workspace => (),
        }
    }

    /// Application runtime.
    pub fn runtime(&self) -> u32 {
        self.start_time.elapsed().as_millis() as u32
    }

    /// Show the application overview.
    pub fn toggle_overview(&mut self) {
        let current_view = self.view;
        let transaction = self.start_transaction();
        transaction.view = match transaction.view.unwrap_or(current_view) {
            View::Overview(_) | View::DragAndDrop(_) => Some(View::Workspace),
            View::Workspace => Some(View::Overview(Overview::default())),
        };
    }

    /// Change the active view.
    fn set_view(&mut self, view: View) {
        self.start_transaction().view = Some(view);
    }

    /// Change the primary window.
    fn set_primary(&mut self, output: &Output, index: impl Into<Option<usize>>) {
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
            primary.borrow_mut().leave(transaction, output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Clear secondary if it's the new primary.
        if weak_window.ptr_eq(&transaction.secondary) {
            transaction.secondary = Weak::new();
        }

        // Set primary and move old one to secondary if it is empty.
        let old_primary = mem::replace(&mut transaction.primary, weak_window);
        if transaction.secondary.strong_count() == 0 {
            transaction.secondary = old_primary;
        }

        transaction.update_dimensions(output);
    }

    /// Change the secondary window.
    fn set_secondary(&mut self, output: &Output, index: impl Into<Option<usize>>) {
        let transaction = self.transaction.get_or_insert(Transaction::new(self));
        let window = index.into().map(|i| &self.windows[i]);

        // Ignore no-ops.
        let weak_window = window.map(Rc::downgrade).unwrap_or_default();
        if weak_window.ptr_eq(&transaction.secondary) {
            return;
        }

        // Update output's visible windows.
        if let Some(secondary) = transaction.secondary.upgrade() {
            secondary.borrow_mut().leave(transaction, output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Clear primary if it's the new secondary.
        if weak_window.ptr_eq(&transaction.primary) {
            transaction.primary = Weak::new();
        }

        // Set secondary and move old one to primary if it is empty.
        let old_secondary = mem::replace(&mut transaction.secondary, weak_window);
        if transaction.primary.strong_count() == 0 {
            transaction.primary = old_secondary;
        }

        transaction.update_dimensions(output);
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

/// Overview view state.
#[derive(Default, Copy, Clone, PartialEq, Debug)]
struct Overview {
    x_offset: f64,
    y_offset: f64,
    last_drag_point: Point<f64, Logical>,
    last_overdrag_step: Option<Instant>,
    drag_direction: Option<Direction>,
    hold_start: Option<Instant>,
}

impl Overview {
    fn new() -> Self {
        Self::default()
    }

    /// Index of the focused window.
    fn focused_index(&self, window_count: usize) -> usize {
        (self.x_offset.min(0.).abs().round() as usize).min(window_count - 1)
    }

    /// Focused window bounds.
    fn focused_bounds(
        &self,
        output_size: Size<i32, Logical>,
        window_count: usize,
    ) -> Rectangle<i32, Logical> {
        let window_size = output_size.scale(FG_OVERVIEW_PERCENTAGE);
        let x = overview_x_position(
            FG_OVERVIEW_PERCENTAGE,
            BG_OVERVIEW_PERCENTAGE,
            output_size.w,
            window_size.w,
            self.focused_index(window_count) as f64 + self.x_offset,
        );
        let y = (output_size.h - window_size.h) / 2;
        Rectangle::from_loc_and_size((x, y), window_size)
    }

    /// Clamp the X/Y offsets.
    ///
    /// This takes overdrag into account and will animate the bounce-back.
    fn clamp_offset(&mut self, window_count: i32) {
        // Limit maximum overdrag.
        let min_offset = -window_count as f64 + 1.;
        self.x_offset = self.x_offset.clamp(min_offset - OVERDRAG_LIMIT, OVERDRAG_LIMIT);

        let last_overdrag_step = match &mut self.last_overdrag_step {
            Some(last_overdrag_step) => last_overdrag_step,
            None => return,
        };

        // Handle bounce-back from overdrag/cancelled application close.

        // Compute framerate-independent delta.
        let delta = last_overdrag_step.elapsed().as_millis() as f64;
        let overdrag_delta = delta / OVERDRAG_ANIMATION_SPEED;
        let close_delta = delta / CLOSE_CANCEL_ANIMATION_SPEED;

        // Overdrag bounce-back.
        if self.x_offset > 0. {
            self.x_offset -= overdrag_delta.min(self.x_offset);
        } else if self.x_offset < min_offset {
            self.x_offset = (self.x_offset + overdrag_delta).min(min_offset);
        }

        // Close window bounce-back.
        self.y_offset -= close_delta.min(self.y_offset.abs()).copysign(self.y_offset);

        *last_overdrag_step = Instant::now();
    }

    /// Render the overview.
    fn draw(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        windows: &[Rc<RefCell<Window>>],
        graphics: &mut Graphics,
    ) {
        let window_count = windows.len() as i32;
        self.clamp_offset(window_count);

        // Create an iterator over all windows in the overview.
        //
        // We start by going over all negative index windows from lowest to highest index and then
        // proceed from highest to lowest index with the positive windows. This ensures that outer
        // windows are rendered below the ones toward the center.
        let min_inc = self.x_offset.round() as i32;
        let max_exc = window_count + self.x_offset.round() as i32;
        let neg_iter = (min_inc..0).zip(0..window_count);
        let pos_iter = (min_inc.max(0)..max_exc).zip(-min_inc.min(0)..window_count).rev();

        // Maximum window size. Bigger windows will be truncated.
        let output_size = output.size();
        let max_size = output_size.scale(FG_OVERVIEW_PERCENTAGE);

        // Window decoration.
        let decoration = graphics.decoration(renderer, output);
        let border_width = Graphics::border_width(output);
        let title_height = Graphics::title_height(output);

        // Render each window at the desired location in the overview.
        for (position, i) in neg_iter.chain(pos_iter) {
            let mut window = windows[i as usize].borrow_mut();

            // Window scale.
            let window_geometry = window.geometry();
            let scale = (max_size.w as f64 / window_geometry.size.w as f64).min(1.);

            // Window boundaries.
            let x_position = overview_x_position(
                FG_OVERVIEW_PERCENTAGE,
                BG_OVERVIEW_PERCENTAGE,
                output_size.w,
                max_size.w,
                position as f64 - self.x_offset.fract().round() + self.x_offset.fract(),
            ) - border_width;
            let y_position = (output_size.h - max_size.h + title_height + border_width) / 2;
            let mut bounds = Rectangle::from_loc_and_size((x_position, y_position), max_size);

            // Offset windows in the process of being closed.
            if position == min_inc.max(0) {
                bounds.loc.y += self.y_offset.round() as i32;
            }

            // Draw decoration.
            let decoration_bounds = Rectangle::from_loc_and_size(
                (bounds.loc.x - border_width, bounds.loc.y - title_height),
                decoration.size(),
            );
            decoration.draw_at(frame, output, decoration_bounds, 1.);

            window.draw(renderer, frame, output, scale, bounds);
        }
    }
}

/// Drag and drop windows into tiling position.
#[derive(Default, Copy, Clone, PartialEq, Debug)]
struct DragAndDrop {
    window_position: Point<f64, Logical>,
    touch_position: Point<f64, Logical>,
    overview_x_offset: f64,
    window_index: usize,
}

impl DragAndDrop {
    fn new(
        touch_position: Point<f64, Logical>,
        overview_x_offset: f64,
        window_index: usize,
    ) -> Self {
        Self {
            overview_x_offset,
            touch_position,
            window_index,
            window_position: Default::default(),
        }
    }

    /// Draw the tiling location picker.
    fn draw(
        &self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        windows: &[Rc<RefCell<Window>>],
        graphics: &mut Graphics,
    ) {
        let output_size = output.size();
        let border_width = Graphics::border_width(output);
        let title_height = Graphics::title_height(output);

        // Calculate window bounds.
        let max_size = output_size.scale(FG_OVERVIEW_PERCENTAGE);
        let x_position = overview_x_position(
            FG_OVERVIEW_PERCENTAGE,
            BG_OVERVIEW_PERCENTAGE,
            output_size.w,
            max_size.w,
            self.overview_x_offset.fract() - self.overview_x_offset.fract().round(),
        ) - border_width;
        let y_position = (output_size.h - max_size.h + title_height + border_width) / 2;
        let mut bounds = Rectangle::from_loc_and_size((x_position, y_position), max_size);
        bounds.loc += self.window_position.to_i32_round();

        // Render decoration for the window.
        let decoration = graphics.decoration(renderer, output);
        let decoration_bounds = Rectangle::from_loc_and_size(
            (bounds.loc.x - border_width, bounds.loc.y - title_height),
            decoration.size(),
        );
        decoration.draw_at(frame, output, decoration_bounds, 1.);

        // Render the window being drag-and-dropped.
        let mut window = windows[self.window_index].borrow_mut();
        window.draw(renderer, frame, output, FG_OVERVIEW_PERCENTAGE, bounds);

        // Set custom OpenGL blending function.
        let _ = renderer.with_context(|_, gl| unsafe {
            gl.BlendFunc(ffi::SRC_ALPHA, ffi::ONE_MINUS_SRC_ALPHA);
        });

        // Get bounds of the drop areas.
        let (primary_bounds, secondary_bounds) = self.drop_bounds(output);

        // Render the drop areas.
        let scale = cmp::max(output_size.w, output_size.h) as f64;
        for bounds in [primary_bounds, secondary_bounds] {
            if bounds.to_f64().contains(self.touch_position) {
                graphics.active_drop_target.draw_at(frame, output, bounds, scale);
            } else {
                graphics.drop_target.draw_at(frame, output, bounds, scale);
            }
        }

        // Reset OpenGL blending function.
        let _ = renderer.with_context(|_, gl| unsafe {
            gl.BlendFunc(ffi::ONE, ffi::ONE_MINUS_SRC_ALPHA);
        });
    }

    /// Bounds for the drop preview areas of the D&D action.
    fn drop_bounds(&self, output: &Output) -> (Rectangle<i32, Logical>, Rectangle<i32, Logical>) {
        let output_size = output.size();
        match output.orientation {
            Orientation::Landscape => {
                let dnd_width = (output_size.w as f64 * DRAG_AND_DROP_PERCENTAGE).round() as i32;
                let size = Size::from((dnd_width, output_size.h));
                let primary = Rectangle::from_loc_and_size((0, 0), size);
                let secondary = Rectangle::from_loc_and_size((output_size.w - dnd_width, 0), size);
                (primary, secondary)
            },
            Orientation::Portrait => {
                let dnd_height = (output_size.h as f64 * DRAG_AND_DROP_PERCENTAGE).round() as i32;
                let size = Size::from((output_size.w, dnd_height));
                let primary = Rectangle::from_loc_and_size((0, 0), size);
                let secondary = Rectangle::from_loc_and_size((0, output_size.h - dnd_height), size);
                (primary, secondary)
            },
        }
    }
}

/// Directional plane.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Direction {
    Horizontal,
    Vertical,
}

/// Atomic changes to [`Windows`].
#[derive(Clone, Debug)]
pub struct Transaction {
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    view: Option<View>,
    start: Instant,
}

impl Transaction {
    fn new(current_state: &Windows) -> Self {
        Self {
            primary: current_state.primary.clone(),
            secondary: current_state.secondary.clone(),
            start: Instant::now(),
            view: None,
        }
    }

    /// Update window dimensions.
    pub fn update_dimensions(&mut self, output: &Output) {
        if let Some(mut primary) = self.primary.upgrade().as_ref().map(|s| s.borrow_mut()) {
            let secondary_visible = self.secondary.strong_count() > 0;
            let rectangle = output.primary_rectangle(secondary_visible);
            primary.update_dimensions(self, rectangle);
        }

        if let Some(mut secondary) = self.secondary.upgrade().as_ref().map(|s| s.borrow_mut()) {
            let rectangle = output.secondary_rectangle();
            secondary.update_dimensions(self, rectangle);
        }
    }
}

/// Atomic changes to [`Window`].
#[derive(Debug)]
struct WindowTransaction {
    rectangle: Rectangle<i32, Logical>,
}

impl WindowTransaction {
    fn new(current_state: &Window) -> Self {
        Self { rectangle: current_state.rectangle }
    }
}

/// Cached window textures.
#[derive(Default, Debug)]
struct TextureCache {
    /// Geometry of all textures combined.
    geometry: Size<i32, Logical>,
    textures: Vec<Texture>,
}

impl TextureCache {
    /// Reset the texture cache.
    fn reset(&mut self, geometry: Size<i32, Logical>) {
        self.geometry = geometry;
        self.textures.clear();
    }

    /// Add a new texture.
    fn push(&mut self, texture: Texture) {
        self.textures.push(texture);
    }
}

/// Wayland client window state.
#[derive(Debug)]
pub struct Window {
    /// Initial size configure status.
    pub initial_configure_sent: bool,

    /// Buffers pending to be imported.
    pub buffers_pending: bool,

    /// Last configure size acked by the client.
    pub acked_size: Size<i32, Logical>,

    /// Desired window dimensions.
    rectangle: Rectangle<i32, Logical>,

    /// Attached surface.
    surface: ToplevelSurface,

    /// Texture cache, storing last window state.
    texture_cache: TextureCache,

    /// Window is currently visible on the output.
    visible: bool,

    /// Transaction for atomic upgrades.
    transaction: Option<WindowTransaction>,
}

impl Window {
    pub fn new(surface: ToplevelSurface) -> Self {
        Window {
            surface,
            initial_configure_sent: Default::default(),
            buffers_pending: Default::default(),
            texture_cache: Default::default(),
            transaction: Default::default(),
            acked_size: Default::default(),
            rectangle: Default::default(),
            visible: Default::default(),
        }
    }

    /// Check if window is visible on the output.
    pub fn visible(&self) -> bool {
        self.visible
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

    /// Send a configure for the latest window properties.
    pub fn reconfigure(&mut self) {
        let result = self.surface.with_pending_state(|state| {
            state.size = Some(match &self.transaction {
                Some(transaction) => transaction.rectangle.size,
                None => self.rectangle.size,
            });

            // Mark window as tiled, using maximized fallback if tiling is unsupported.
            if self.surface.version() >= 2 {
                state.states.set(State::TiledBottom);
                state.states.set(State::TiledRight);
                state.states.set(State::TiledLeft);
                state.states.set(State::TiledTop);
            } else {
                state.states.set(State::Maximized);
            }

            // Always use server-side decoration.
            state.decoration_mode = Some(DecorationMode::ServerSide);
        });

        if result.is_ok() {
            self.surface.send_configure();
        }
    }

    /// Change the window dimensions.
    fn update_dimensions(&mut self, transaction: &Transaction, rectangle: Rectangle<i32, Logical>) {
        // Prevent redundant configure events.
        let transaction = self.start_transaction(transaction);
        let old_ractangle = mem::replace(&mut transaction.rectangle, rectangle);
        if transaction.rectangle != old_ractangle && self.initial_configure_sent {
            self.reconfigure();
        }
    }

    /// Send output enter event to this window's surfaces.
    fn enter(&mut self, output: &Output) {
        self.with_surfaces(|surface, _| output.enter(surface));
        self.visible = true;
    }

    /// Send output leave event to this window's surfaces.
    fn leave(&mut self, transaction: &Transaction, output: &Output) {
        self.with_surfaces(|surface, _| output.leave(surface));
        self.visible = false;

        // Resize to fullscreen for app overview.
        let rectangle = Rectangle::from_loc_and_size((0, 0), output.size());
        self.update_dimensions(transaction, rectangle);
    }

    /// Render this window's buffers.
    ///
    /// If no location is specified, the textures cached location will be used.
    fn draw(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        scale: f64,
        bounds: impl Into<Option<Rectangle<i32, Logical>>>,
    ) {
        // Skip updating windows during transactions.
        if self.transaction.is_none() && self.buffers_pending {
            self.import_buffers(renderer);
        }

        let bounds = bounds.into().unwrap_or_else(|| {
            // Center window inside its space.
            let x_offset = ((self.rectangle.size.w - self.texture_cache.geometry.w) / 2).max(0);
            let y_offset = ((self.rectangle.size.h - self.texture_cache.geometry.h) / 2).max(0);
            let loc = self.rectangle.loc + Size::from((x_offset, y_offset));
            Rectangle::from_loc_and_size(loc, output.size())
        });

        for texture in &self.texture_cache.textures {
            texture.draw_at(frame, output, bounds, scale);
        }
    }

    /// Import the buffers of all surfaces into the renderer.
    fn import_buffers(&mut self, renderer: &mut Gles2Renderer) {
        // Ensure there is a drawable surface present.
        let wl_surface = match self.surface.get_surface() {
            Some(surface) => surface,
            None => return,
        };

        let geometry = self.geometry();
        self.texture_cache.reset(geometry.size);
        self.buffers_pending = false;

        compositor::with_surface_tree_upward(
            wl_surface,
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
                    let texture = Texture::new(texture.clone(), data.size(), location, data.scale);
                    self.texture_cache.push(texture);
                    return TraversalAction::DoChildren(location);
                }

                // Import and cache the buffer.

                let buffer = match &data.buffer {
                    Some(buffer) => buffer,
                    None => return TraversalAction::SkipChildren,
                };

                let damage: Vec<_> = surface_data
                    .cached_state
                    .current::<SurfaceAttributes>()
                    .damage
                    .iter()
                    .map(|damage| match damage {
                        Damage::Buffer(rect) => *rect,
                        Damage::Surface(rect) => rect.to_buffer(data.scale),
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
                        let texture = Texture::new(texture, data.size(), location, data.scale);
                        self.texture_cache.push(texture);

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

    /// Geometry of the window's visible bounds.
    fn geometry(&self) -> Rectangle<i32, Logical> {
        self.surface
            .get_surface()
            .and_then(|surface| {
                compositor::with_states(surface, |states| {
                    states.cached_state.current::<SurfaceCachedState>().geometry
                })
                .ok()
                .flatten()
            })
            .unwrap_or_default()
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

    /// Create a new transaction, or access the active one.
    ///
    /// Takes in a transaction as parameter to ensure the window will not get stuck in the frozen
    /// state indefinitely.
    fn start_transaction(&mut self, _transaction: &Transaction) -> &mut WindowTransaction {
        self.transaction.get_or_insert(WindowTransaction::new(self))
    }

    /// Apply all staged changes if there is a transaction.
    fn apply_transaction(&mut self) {
        let transaction = match self.transaction.take() {
            Some(transaction) => transaction,
            None => return,
        };

        self.rectangle = transaction.rectangle;
    }
}

/// Calculate the X coordinate of a window in the application overview based on its position.
fn overview_x_position(
    fg_percentage: f64,
    bg_percentage: f64,
    output_width: i32,
    window_width: i32,
    position: f64,
) -> i32 {
    let bg_space_size = output_width as f64 * (1. - fg_percentage) * 0.5;
    let next_space_size = bg_space_size * (1. - bg_percentage).powf(position.abs());
    let next_space_size = next_space_size.round() as i32;

    match position.partial_cmp(&0.) {
        Some(Ordering::Less) => next_space_size,
        Some(Ordering::Greater) => output_width - window_width - next_space_size,
        _ => bg_space_size.round() as i32,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn overview_position() {
        assert_eq!(overview_x_position(0.5, 0.5, 100, 50, -2.), 6);
        assert_eq!(overview_x_position(0.5, 0.5, 100, 50, -1.), 13);
        assert_eq!(overview_x_position(0.5, 0.5, 100, 50, 0.), 25);
        assert_eq!(overview_x_position(0.5, 0.5, 100, 50, 1.), 37);
        assert_eq!(overview_x_position(0.5, 0.5, 100, 50, 2.), 44);

        assert_eq!(overview_x_position(0.5, 0.75, 100, 50, -2.), 2);
        assert_eq!(overview_x_position(0.5, 0.75, 100, 50, -1.), 6);
        assert_eq!(overview_x_position(0.5, 0.75, 100, 50, 0.), 25);
        assert_eq!(overview_x_position(0.5, 0.75, 100, 50, 1.), 44);
        assert_eq!(overview_x_position(0.5, 0.75, 100, 50, 2.), 48);

        assert_eq!(overview_x_position(0.75, 0.75, 100, 50, -2.), 1);
        assert_eq!(overview_x_position(0.75, 0.75, 100, 50, -1.), 3);
        assert_eq!(overview_x_position(0.75, 0.75, 100, 50, 0.), 13);
        assert_eq!(overview_x_position(0.75, 0.75, 100, 50, 1.), 47);
        assert_eq!(overview_x_position(0.75, 0.75, 100, 50, 2.), 49);
    }
}
