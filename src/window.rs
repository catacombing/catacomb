//! Window management.

use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};
use std::{cmp, mem};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, BufferType, Frame, ImportAll, Transform};
use smithay::reexports::wayland_protocols::unstable::xdg_decoration;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor::{
    self, Damage, SubsurfaceCachedState, SurfaceAttributes, SurfaceData, TraversalAction,
};
use smithay::wayland::shell::xdg::{SurfaceCachedState, ToplevelSurface};
use wayland_protocols::xdg_shell::server::xdg_toplevel::State;
use xdg_decoration::v1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;

use crate::output::Output;
use crate::shell::SurfaceBuffer;

/// Maximum time before a transaction is cancelled.
const MAX_TRANSACTION_DURATION: Duration = Duration::from_millis(200);

/// Percentage of output width reserved for the main window in the application overview.
const FG_OVERVIEW_PERCENTAGE: f64 = 0.75;

/// Percentage of remaining space reserved for background windows in the application overview.
const BG_OVERVIEW_PERCENTAGE: f64 = 0.5;

/// Animation speed for the return from overdrag, lower means faster.
const OVERDRAG_ANIMATION_SPEED: f64 = 25.;

/// Maximum amount of overdrag before inputs are ignored.
const OVERDRAG_LIMIT: f64 = 3.;

/// Compositor window arrangements.
#[derive(Copy, Clone, PartialEq, Debug)]
enum View {
    /// List of all open windows.
    Overview { offset: f64, last_overdrag_step: Option<Instant> },
    /// Currently active windows.
    Workspace,
}

impl Default for View {
    fn default() -> Self {
        View::Workspace
    }
}

/// Container tracking all known clients.
#[derive(Debug, Clone)]
pub struct Windows {
    windows: Vec<Rc<RefCell<Window>>>,
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    transaction: Option<Transaction>,
    start_time: Instant,
    view: View,
}

impl Default for Windows {
    fn default() -> Self {
        Self {
            start_time: Instant::now(),
            transaction: Default::default(),
            secondary: Default::default(),
            windows: Default::default(),
            primary: Default::default(),
            view: Default::default(),
        }
    }
}

impl Windows {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new window.
    pub fn add(&mut self, surface: ToplevelSurface, output: &Output) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        if self.primary.strong_count() > 0 && self.secondary.strong_count() == 0 {
            self.set_secondary(&window, output);
        } else {
            self.set_primary(&window, output);
            self.set_secondary(None, output);
        }
        self.windows.push(window);
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
    pub fn with_visible<F: FnMut(&mut Window)>(&mut self, mut fun: F) {
        for window in self.primary.upgrade().iter_mut().chain(&mut self.secondary.upgrade()) {
            fun(&mut window.borrow_mut());
        }
    }

    /// Draw the current window state.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, output: &Output) {
        let (offset, last_overdrag_step) = match &mut self.view {
            View::Workspace => {
                self.with_visible(|window| window.draw(renderer, frame, output, 1., None));
                return;
            },
            View::Overview { offset, last_overdrag_step } => (offset, last_overdrag_step),
        };

        // Handle bounce-back animation from overdrag.
        let window_count = self.windows.len() as i32;
        let min_offset = -window_count as f64 + 1.;
        if let Some(last_overdrag_step) = last_overdrag_step {
            let delta = last_overdrag_step.elapsed().as_millis() as f64 / OVERDRAG_ANIMATION_SPEED;
            if *offset > 0. {
                *offset -= delta.min(*offset);
            } else if *offset < min_offset {
                *offset = (*offset + delta).min(min_offset);
            }
            *last_overdrag_step = Instant::now();
        }

        *offset = offset.clamp(min_offset - OVERDRAG_LIMIT, OVERDRAG_LIMIT);

        // Create an iterator over all windows in the overview.
        //
        // We start by going over all negative index windows from lowest to highest index and then
        // proceed from highest to lowest index with the positive windows. This ensures that outer
        // windows are rendered below the ones toward the center.
        let min_inc = offset.round() as i32;
        let max_exc = window_count + offset.round() as i32;
        let neg_iter = (min_inc..0).zip(0..window_count);
        let pos_iter = (min_inc.max(0)..max_exc).zip(-min_inc.min(0)..window_count).rev();

        // Maximum window size. Bigger windows will be truncated.
        let output_size = output.size();
        let max_size = scale_size(output_size, FG_OVERVIEW_PERCENTAGE);

        // Render each window at the desired location in the overview.
        for (position, i) in neg_iter.chain(pos_iter) {
            let mut window = self.windows[i as usize].borrow_mut();

            // Window scale.
            let window_geometry = window.geometry();
            let scale = (max_size.w as f64 / window_geometry.size.w as f64).min(1.);

            // Window boundaries.
            let x_position = overview_x_position(
                FG_OVERVIEW_PERCENTAGE,
                BG_OVERVIEW_PERCENTAGE,
                output_size.w,
                max_size.w,
                position as f64 - offset.fract().round() + offset.fract(),
            );
            let y_position = (output_size.h - max_size.h) / 2;
            let bounds = Rectangle::from_loc_and_size((x_position, y_position), max_size);

            window.draw(renderer, frame, output, scale, Some(bounds));
        }
    }

    /// Refresh the client list.
    ///
    /// This function will remove all dead windows and resize remaining windows accordingly.
    /// Subsequently new frames are requested from all visible windows.
    pub fn refresh(&mut self, output: &Output) {
        // Request frames for visible windows.
        if self.view == View::Workspace {
            let runtime = self.runtime();
            self.with_visible(|window| window.request_frame(runtime));
        }

        // Start transaction if any window died.
        if self.windows.iter().all(|window| window.borrow().surface.alive()) {
            return;
        }
        let transaction = self.start_transaction();

        // Remove dead primary/secondary windows.
        if transaction.secondary.upgrade().map_or(false, |window| !window.borrow().surface.alive())
        {
            transaction.secondary = Weak::new();
        }
        if transaction.primary.upgrade().map_or(false, |window| !window.borrow().surface.alive()) {
            transaction.primary = mem::take(&mut transaction.secondary);
        }

        transaction.update_dimensions(output);
    }

    /// Create a new transaction, or access the active one.
    pub fn start_transaction(&mut self) -> &mut Transaction {
        self.transaction.get_or_insert(Transaction::new(self))
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

        // Execute the transaction.
        for i in (0..self.windows.len()).rev() {
            if self.windows[i].borrow().surface.alive() {
                self.windows[i].borrow_mut().apply_transaction();
            } else {
                self.windows.swap_remove(i);
            }
        }

        let transaction = self.transaction.take().unwrap();
        self.secondary = transaction.secondary;
        self.primary = transaction.primary;
        self.view = transaction.view;
    }

    /// Toggle the active view.
    pub fn toggle_view(&mut self) {
        let transaction = self.start_transaction();
        transaction.view = match transaction.view {
            View::Workspace => View::Overview { last_overdrag_step: None, offset: 0. },
            View::Overview { .. } => View::Workspace,
        };
    }

    /// Hand quick touch input.
    pub fn on_tap(&mut self, output: &Output, point: Point<f64, Logical>) {
        let offset = match self.view {
            View::Overview { offset, .. } => offset,
            View::Workspace => return,
        };

        // Calculate focused overview window bounds.
        let output_size = output.size();
        let window_size = scale_size(output_size, FG_OVERVIEW_PERCENTAGE);
        let index = (offset.min(0.).abs().round() as usize).min(self.windows.len() - 1);
        let x = overview_x_position(
            FG_OVERVIEW_PERCENTAGE,
            BG_OVERVIEW_PERCENTAGE,
            output_size.w,
            window_size.w,
            index as f64 + offset,
        );
        let y = (output_size.h - window_size.h) / 2;
        let window_bounds = Rectangle::from_loc_and_size((x, y), window_size);

        // Open the window as sole primary.
        if window_bounds.contains(point.to_i32_round()) {
            let window = self.windows[index].clone();
            self.set_primary(&window, output);
            self.set_secondary(None, output);
            self.toggle_view();
        }
    }

    /// Handle touch drag.
    pub fn on_drag(&mut self, delta: f64) {
        if let View::Overview { offset, last_overdrag_step } = &mut self.view {
            *last_overdrag_step = None;
            *offset += delta;
        }
    }

    /// Handle touch release.
    pub fn on_drag_release(&mut self) {
        if let View::Overview { last_overdrag_step, .. } = &mut self.view {
            *last_overdrag_step = Some(Instant::now());
        }
    }

    /// Application runtime.
    pub fn runtime(&self) -> u32 {
        self.start_time.elapsed().as_millis() as u32
    }

    /// Change the primary window.
    fn set_primary<'a>(
        &mut self,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
        output: &Output,
    ) {
        let transaction = self.start_transaction();

        // Update output's visible windows.
        let window = window.into();
        if let Some(primary) = transaction.primary.upgrade() {
            primary.borrow_mut().leave(transaction, output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Set primary and recompute window dimensions.
        transaction.primary =
            window.map(Rc::downgrade).unwrap_or_else(|| mem::take(&mut transaction.secondary));
        transaction.update_dimensions(output);
    }

    /// Change the secondary window.
    fn set_secondary<'a>(
        &mut self,
        window: impl Into<Option<&'a Rc<RefCell<Window>>>>,
        output: &Output,
    ) {
        let transaction = self.start_transaction();

        // Update output's visible windows.
        let window = window.into();
        if let Some(secondary) = transaction.secondary.upgrade() {
            secondary.borrow_mut().leave(transaction, output);
        }
        if let Some(window) = &window {
            window.borrow_mut().enter(output);
        }

        // Set primary and recompute window dimensions.
        transaction.secondary = window.map(Rc::downgrade).unwrap_or_default();
        transaction.update_dimensions(output);
    }
}

/// Atomic changes to [`Windows`].
#[derive(Clone, Debug)]
pub struct Transaction {
    primary: Weak<RefCell<Window>>,
    secondary: Weak<RefCell<Window>>,
    start: Instant,
    view: View,
}

impl Transaction {
    fn new(current_state: &Windows) -> Self {
        Self {
            primary: current_state.primary.clone(),
            secondary: current_state.secondary.clone(),
            view: current_state.view,
            start: Instant::now(),
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

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
#[derive(Clone, Debug)]
struct Texture {
    dimensions: Size<i32, Logical>,
    location: Point<i32, Logical>,
    texture: Rc<Gles2Texture>,
    scale: i32,
}

impl Texture {
    fn new(
        texture: Rc<Gles2Texture>,
        dimensions: Size<i32, Logical>,
        location: Point<i32, Logical>,
        scale: i32,
    ) -> Self {
        Self { texture, dimensions, location, scale }
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

            // Always use server-side decorations.
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
        let mut rectangle = self.start_transaction(transaction).rectangle;
        rectangle.size = output.size();
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
        bounds: Option<Rectangle<i32, Logical>>,
    ) {
        // Skip updating windows during transactions.
        if self.transaction.is_none() && self.buffers_pending {
            self.import_buffers(renderer);
        }

        let bounds = bounds.unwrap_or_else(|| {
            // Center window inside its space.
            let x_offset = ((self.rectangle.size.w - self.texture_cache.geometry.w) / 2).max(0);
            let y_offset = ((self.rectangle.size.h - self.texture_cache.geometry.h) / 2).max(0);
            let loc = self.rectangle.loc + Size::from((x_offset, y_offset));
            Rectangle::from_loc_and_size(loc, output.size())
        });
        self.draw_at(frame, output, bounds, scale);
    }

    /// Render the window at the specified location.
    ///
    /// Using the `window_bounds` and `window_scale` parameters, it is possible to scale the
    /// surface and truncate it to be within the specified window bounds. The scaling will always
    /// take part **before** the truncation.
    fn draw_at(
        &self,
        frame: &mut Gles2Frame,
        output: &Output,
        window_bounds: Rectangle<i32, Logical>,
        window_scale: f64,
    ) {
        let scaled_window_bounds = scale_size(window_bounds.size, 1. / window_scale);
        for Texture { texture, dimensions, location, scale } in &self.texture_cache.textures {
            // Skip textures completely outside of the window bounds.
            if location.x >= scaled_window_bounds.w || location.y >= scaled_window_bounds.h {
                continue;
            }

            // Truncate source size based on window bounds.
            let surface_bounds = scaled_window_bounds - *location;
            let src_size = Size::from((
                cmp::min(dimensions.w, surface_bounds.w),
                cmp::min(dimensions.h, surface_bounds.h),
            ));
            let src = Rectangle::from_loc_and_size((0, 0), src_size);

            // Scale output size based on window scale.
            let location = window_bounds.loc + scale_location(*location, window_scale);
            let dest = Rectangle::from_loc_and_size(location, scale_size(src_size, window_scale));

            let _ = frame.render_texture_from_to(
                texture,
                src.to_buffer(*scale),
                dest.to_f64().to_physical(output.scale),
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

/// Scale a size by a scaling factor.
fn scale_size(size: Size<i32, Logical>, scale: f64) -> Size<i32, Logical> {
    Size::from((size.w as f64 * scale, size.h as f64 * scale)).to_i32_round()
}

/// Scale a location by a scaling factor.
fn scale_location(location: Point<i32, Logical>, scale: f64) -> Point<i32, Logical> {
    Point::from((location.x as f64 * scale, location.y as f64 * scale)).to_i32_round()
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
