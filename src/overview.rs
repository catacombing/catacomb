//! Application overview.

use std::cell::RefCell;
use std::cmp;
use std::rc::Rc;
use std::time::Instant;

use smithay::backend::renderer::gles2::{ffi, Gles2Frame, Gles2Renderer};
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use smithay::utils::{Logical, Point, Rectangle, Size};

use crate::catacomb::Catacomb;
use crate::drawing::Graphics;
use crate::geometry::Vector;
use crate::input::HOLD_DURATION;
use crate::output::Output;
use crate::windows::window::Window;

/// Percentage of output width reserved for the main window in the application
/// overview.
const FG_OVERVIEW_PERCENTAGE: f64 = 0.75;

/// Percentage of output width reserved for the secondary windows in the
/// application overview.
const BG_OVERVIEW_PERCENTAGE: f64 = 0.7;

/// Percentage of the screen for the drop highlight areas.
const DRAG_AND_DROP_PERCENTAGE: f64 = 0.3;

/// Percentage of the output height a window can be moved before closing it in
/// the overview.
const OVERVIEW_CLOSE_DISTANCE: f64 = 0.25;

/// Animation speed for the return from close, lower means faster.
const CLOSE_CANCEL_ANIMATION_SPEED: f64 = 0.3;

/// Animation speed for the return from overdrag, lower means faster.
const OVERDRAG_ANIMATION_SPEED: f64 = 25.;

/// Spacing between windows in the overview, as percentage from overview width.
const OVERVIEW_SPACING_PERCENTAGE: f64 = 0.75;

/// Maximum amount of overdrag before inputs are ignored.
const OVERDRAG_LIMIT: f64 = 3.;

/// Overview view state.
#[derive(Default, Copy, Clone, PartialEq, Debug)]
pub struct Overview {
    pub x_offset: f64,
    pub y_offset: f64,
    pub hold_timer: Option<RegistrationToken>,
    pub last_drag_point: Point<f64, Logical>,
    pub last_overdrag_step: Option<Instant>,
    pub drag_direction: Option<Direction>,
}

impl Overview {
    pub fn new() -> Self {
        Self::default()
    }

    /// Start timer for D&D touch hold.
    pub fn start_hold(&mut self, event_loop: &LoopHandle<'static, Catacomb>) {
        // Ensure no timer is currently active.
        self.cancel_hold(event_loop);

        // Start a new timer.
        let timer = Timer::from_duration(HOLD_DURATION);
        let hold_timer = event_loop
            .insert_source(timer, |_, _, catacomb| {
                catacomb.windows.start_dnd();
                catacomb.unstall();
                TimeoutAction::Drop
            })
            .expect("insert D&D hold timer");
        self.hold_timer = Some(hold_timer);
    }

    /// Cancel the D&D touch hold timer.
    pub fn cancel_hold(&mut self, event_loop: &LoopHandle<'static, Catacomb>) {
        if let Some(timer) = self.hold_timer.take() {
            event_loop.remove(timer);
        }
    }

    /// Index of the focused window.
    pub fn focused_index(&self, window_count: usize) -> usize {
        (self.x_offset.min(0.).abs().round() as usize).min(window_count - 1)
    }

    /// Focused window bounds.
    pub fn focused_bounds(&self, output: &Output) -> Rectangle<i32, Logical> {
        let available = output.available_overview();
        OverviewPosition::new(available, self.x_offset, 0., self.x_offset).bounds
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
    pub fn draw(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        windows: &[Rc<RefCell<Window>>],
    ) {
        let window_count = windows.len() as i32;
        self.clamp_offset(window_count);

        let available = output.available_overview();

        // Draw up to three visible windows (center and one to each side).
        let mut offset = self.x_offset - 1.;
        while offset < self.x_offset + 2. {
            let window_index = usize::try_from(-offset.round() as isize);

            if let Some(window) = window_index.ok().and_then(|i| windows.get(i)) {
                let position =
                    OverviewPosition::new(available, self.x_offset, self.y_offset, offset);

                // Draw the window.
                let mut window = window.borrow_mut();
                window.draw(renderer, frame, output, position.scale, position.bounds, None);
            }

            offset += 1.;
        }
    }

    /// Check if the active window has exceeded the minimum close distance.
    pub fn should_close(&self, output: &Output) -> bool {
        let close_distance = output.available_overview().size.h as f64 * OVERVIEW_CLOSE_DISTANCE;
        self.y_offset.abs() >= close_distance
    }

    /// Check if the overdrag has run into a hard limit.
    pub fn overdrag_limited(&self, window_count: usize) -> bool {
        let min_offset = -(window_count as f64) + 1.;
        self.x_offset <= min_offset - OVERDRAG_LIMIT || self.x_offset >= OVERDRAG_LIMIT
    }

    /// Check if overview animations are active.
    pub fn animating_drag(&self, window_count: usize) -> bool {
        let min_offset = -(window_count as f64) + 1.;
        self.x_offset > 0. || self.x_offset < min_offset || self.y_offset != 0.
    }
}

/// Drag and drop windows into tiling position.
#[derive(Default, Copy, Clone, PartialEq, Debug)]
pub struct DragAndDrop {
    pub window_position: Point<f64, Logical>,
    pub touch_position: Point<f64, Logical>,
    pub overview_x_offset: f64,
    pub window_index: usize,
}

impl DragAndDrop {
    pub fn new(
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
    pub fn draw(
        &self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
        windows: &[Rc<RefCell<Window>>],
        graphics: &mut Graphics,
    ) {
        let available = output.available_overview();

        // Calculate window bounds.
        let x_offset = self.overview_x_offset;
        let mut bounds = OverviewPosition::new(available, x_offset, 0., x_offset).bounds;

        // Offset by dragged distance.
        bounds.loc += self.window_position.to_i32_round();

        // Render the window being drag-and-dropped.
        let mut window = windows[self.window_index].borrow_mut();
        window.draw(renderer, frame, output, FG_OVERVIEW_PERCENTAGE, bounds, None);

        // Set custom OpenGL blending function.
        let _ = renderer.with_context(|_, gl| unsafe {
            gl.BlendFunc(ffi::SRC_ALPHA, ffi::ONE_MINUS_SRC_ALPHA);
        });

        // Get bounds of the drop areas.
        let (primary_bounds, secondary_bounds) = self.drop_bounds(output);

        // Render the drop areas.
        let scale = cmp::max(available.size.w, available.size.h) as f64;
        for bounds in [primary_bounds, secondary_bounds] {
            if bounds.to_f64().contains(self.touch_position) {
                graphics.active_drop_target(renderer).draw_at(frame, output, bounds, scale, None);
            } else {
                graphics.drop_target(renderer).draw_at(frame, output, bounds, scale, None);
            }
        }

        // Reset OpenGL blending function.
        let _ = renderer.with_context(|_, gl| unsafe {
            gl.BlendFunc(ffi::ONE, ffi::ONE_MINUS_SRC_ALPHA);
        });
    }

    /// Bounds for the drop preview areas of the D&D action.
    pub fn drop_bounds(
        &self,
        output: &Output,
    ) -> (Rectangle<i32, Logical>, Rectangle<i32, Logical>) {
        let available = output.available_overview();
        if available.size.h > available.size.w {
            let dnd_height = (available.size.h as f64 * DRAG_AND_DROP_PERCENTAGE).round() as i32;
            let size = Size::from((available.size.w, dnd_height));
            let primary = Rectangle::from_loc_and_size(available.loc, size);

            let mut secondary = primary;
            secondary.loc.y += available.size.h - dnd_height;

            (primary, secondary)
        } else {
            let dnd_width = (available.size.w as f64 * DRAG_AND_DROP_PERCENTAGE).round() as i32;
            let size = Size::from((dnd_width, available.size.h));
            let primary = Rectangle::from_loc_and_size(available.loc, size);

            let mut secondary = primary;
            secondary.loc.x += available.size.w - dnd_width;

            (primary, secondary)
        }
    }
}

/// Directional plane.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Direction {
    Horizontal,
    Vertical,
}

/// Window placed in the application overview.
#[derive(Debug)]
struct OverviewPosition {
    bounds: Rectangle<i32, Logical>,
    scale: f64,
}

impl OverviewPosition {
    /// Calculate the rectangle for a window in the application overview.
    fn new(
        available_rect: Rectangle<i32, Logical>,
        center_offset_x: f64,
        center_offset_y: f64,
        window_offset: f64,
    ) -> Self {
        // Calculate window's distance from the center of the overview.
        let delta = center_offset_x - window_offset.round();

        // Calculate the window's scale.
        let scale = BG_OVERVIEW_PERCENTAGE
            + (FG_OVERVIEW_PERCENTAGE - BG_OVERVIEW_PERCENTAGE) * (1. - delta.abs());

        // Calculate the window's size and position.
        let bounds_size = available_rect.size.scale(scale);
        let bounds_loc = available_rect.loc + (available_rect.size - bounds_size).scale(0.5);
        let mut bounds = Rectangle::from_loc_and_size(bounds_loc, bounds_size);
        bounds.loc.x += (delta * available_rect.size.w as f64 * OVERVIEW_SPACING_PERCENTAGE) as i32;

        // Offset windows in the process of being closed.
        if (window_offset - center_offset_x).abs() < f64::EPSILON {
            bounds.loc.y += center_offset_y.round() as i32;
        }

        Self { bounds, scale }
    }
}
