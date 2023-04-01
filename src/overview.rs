//! Application overview.

use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Instant;

use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use smithay::utils::{Logical, Point, Rectangle, Size};

use crate::catacomb::Catacomb;
use crate::drawing::{CatacombElement, Graphics};
use crate::geometry::Vector;
use crate::input::HOLD_DURATION;
use crate::output::{Canvas, Output};
use crate::windows::layout::{LayoutPosition, Layouts};
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
const VERTICAL_DRAG_ANIMATION_SPEED: f64 = 0.3;

/// Animation speed for the return to nearest integer x-offset, lower means
/// faster.
const HORIZONTAL_DRAG_ANIMATION_SPEED: f64 = 75.;

/// Spacing between windows in the overview, as percentage from overview width.
const OVERVIEW_SPACING_PERCENTAGE: f64 = 0.75;

/// Maximum amount of overdrag before inputs are ignored.
const OVERDRAG_LIMIT: f64 = 0.25;

/// Overview view state.
#[derive(Debug)]
pub struct Overview {
    pub hold_timer: Option<RegistrationToken>,
    pub last_drag_point: Point<f64, Logical>,
    pub last_animation_step: Option<Instant>,
    pub drag_action: DragAction,
    pub x_offset: f64,
    pub y_offset: f64,
}

impl Overview {
    pub fn new(active_offset: f64) -> Self {
        Self {
            x_offset: active_offset,
            last_animation_step: Default::default(),
            last_drag_point: Default::default(),
            drag_action: Default::default(),
            hold_timer: Default::default(),
            y_offset: Default::default(),
        }
    }

    /// Start timer for D&D touch hold.
    pub fn start_hold(
        &mut self,
        event_loop: &LoopHandle<'static, Catacomb>,
        layout_position: LayoutPosition,
    ) {
        // Ensure no timer is currently active.
        self.cancel_hold(event_loop);

        // Start a new timer.
        let timer = Timer::from_duration(HOLD_DURATION);
        let hold_timer = event_loop
            .insert_source(timer, move |_, _, catacomb| {
                catacomb.windows.start_dnd(layout_position);
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

    /// Get layout position at the specified point.
    pub fn layout_position(
        &self,
        canvas: &Canvas,
        layouts: &Layouts,
        point: Point<f64, Logical>,
    ) -> Option<LayoutPosition> {
        let available = canvas.available_overview();

        let mut offset = self.x_offset - 1.;
        while offset < self.x_offset + 2. {
            let position = OverviewPosition::new(available, self.x_offset, offset);
            if position.bounds.to_f64().contains(point) {
                let layout_index = usize::try_from(-offset.round() as isize).ok()?;
                let layout = layouts.get(layout_index)?;

                // Check if click was within secondary window.
                if layout.secondary().is_some()
                    && position.secondary_bounds().to_f64().contains(point)
                {
                    return Some(LayoutPosition::new(layout_index, true));
                }

                // Check if click was within primary window.
                if layout.primary().is_some() && position.bounds.to_f64().contains(point) {
                    return Some(LayoutPosition::new(layout_index, false));
                }
            }

            offset += 1.;
        }

        None
    }

    /// Clamp the X/Y offsets.
    ///
    /// This takes overdrag into account and will animate the bounce-back.
    fn clamp_offset(&mut self, canvas: &Canvas, window_count: i32) {
        // Limit maximum overdrag.
        let min_offset = -window_count as f64 + 1.;
        self.x_offset = self.x_offset.clamp(min_offset - OVERDRAG_LIMIT, OVERDRAG_LIMIT);

        let last_animation_step = match &mut self.last_animation_step {
            Some(last_animation_step) => last_animation_step,
            None => return,
        };

        // Handle horizontal and vertical bounce-back.

        // Compute framerate-independent delta.
        let delta = last_animation_step.elapsed().as_millis() as f64;
        let horizontal_delta = delta / HORIZONTAL_DRAG_ANIMATION_SPEED;
        let vertical_delta = delta / VERTICAL_DRAG_ANIMATION_SPEED;

        // Horizontal bounce-back to closes valid integer offset.
        let target = self.x_offset.round().clamp(min_offset, 0.);
        let fract = self.x_offset.fract();
        if self.x_offset > 0. || fract <= -0.5 {
            self.x_offset = (self.x_offset - horizontal_delta).max(target);
        } else if self.x_offset < min_offset || fract <= f64::EPSILON {
            self.x_offset = (self.x_offset + horizontal_delta).min(target);
        }

        // Check if window is past the point of no return for closing.
        let close_distance = canvas.available_overview().size.h as f64 * OVERVIEW_CLOSE_DISTANCE;
        let is_closing = self.y_offset.abs() >= close_distance;

        if is_closing {
            // Continue animation until window is offscreen.
            self.y_offset += vertical_delta.copysign(self.y_offset);
        } else {
            // Close window bounce-back.
            self.y_offset -= vertical_delta.min(self.y_offset.abs()).copysign(self.y_offset);
        }

        *last_animation_step = Instant::now();
    }

    /// Get textures for rendering the overview.
    pub fn textures(
        &mut self,
        textures: &mut Vec<CatacombElement>,
        output: &Output,
        canvas: &Canvas,
        layouts: &Layouts,
    ) {
        let layout_count = layouts.len() as i32;
        self.clamp_offset(canvas, layout_count);

        let available = canvas.available_overview();

        // Draw up to three visible windows (center and one to each side).
        let mut offset = self.x_offset - 1.;
        while offset < self.x_offset + 2. {
            let layout_index = usize::try_from(-offset.round() as isize).ok();

            // Get layout at offset index.
            let layout = match layout_index.and_then(|i| layouts.get(i)) {
                Some(layout) => layout,
                None => {
                    offset += 1.;
                    continue;
                },
            };

            let position = OverviewPosition::new(available, self.x_offset, offset);

            // Draw the primary window.
            if let Some(primary) = layout.primary() {
                // Offset bounds for closing windows.
                let mut bounds = position.bounds;
                self.handle_closing(output, canvas, layouts, primary, &mut bounds, true);

                let primary = primary.borrow();
                let scale = position.scale;
                let loc = bounds.loc + primary.internal_offset().scale(scale);
                primary.textures(textures, canvas.scale(), scale, loc);
            }

            // Draw the secondary window.
            if let Some(secondary) = layout.secondary() {
                // Offset bounds for closing windows.
                let mut bounds = position.secondary_bounds();
                self.handle_closing(output, canvas, layouts, secondary, &mut bounds, false);

                let secondary = secondary.borrow();
                let scale = position.scale;
                let loc = bounds.loc + secondary.internal_offset().scale(scale);
                secondary.textures(textures, canvas.scale(), scale, loc);
            }

            offset += 1.;
        }
    }

    /// Handle closing window for drawing the overview.
    fn handle_closing(
        &mut self,
        output: &Output,
        canvas: &Canvas,
        layouts: &Layouts,
        window: &Rc<RefCell<Window>>,
        bounds: &mut Rectangle<i32, Logical>,
        is_primary: bool,
    ) {
        // Check if this window is being closed.
        let closing_window = self.drag_action.closing_window();
        if !Weak::ptr_eq(&closing_window, &Rc::downgrade(window)) {
            return;
        }

        // Prevent dragging primary/secondary across from each other.
        if is_primary {
            self.y_offset = self.y_offset.min(0.);
        } else {
            self.y_offset = self.y_offset.max(0.);
        }

        // Offset window position vertically while getting dragged to close.
        bounds.loc.y += self.y_offset.round() as i32;

        // Kill the window if it has moved completely offscreen.
        if !Rectangle::from_loc_and_size((0, 0), canvas.size()).overlaps(*bounds) {
            let surface = {
                let mut window = window.borrow_mut();
                window.kill();
                window.surface.clone()
            };
            layouts.reap(output, &surface);
        }
    }

    /// Check if the overdrag has run into a hard limit.
    pub fn overdrag_limited(&self, window_count: usize) -> bool {
        let min_offset = -(window_count as f64) + 1.;
        self.x_offset <= min_offset - OVERDRAG_LIMIT || self.x_offset >= OVERDRAG_LIMIT
    }

    /// Check if overview animations are active.
    pub fn animating(&self, window_count: usize) -> bool {
        let min_offset = -(window_count as f64) + 1.;
        self.x_offset > 0.
            || self.x_offset < min_offset
            || self.y_offset != 0.
            || self.x_offset.fract().abs() > f64::EPSILON
    }
}

/// Drag and drop windows into tiling position.
#[derive(Clone, Debug)]
pub struct DragAndDrop {
    pub window_position: Point<f64, Logical>,
    pub touch_position: Point<f64, Logical>,
    pub window: Rc<RefCell<Window>>,
    pub overview_x_offset: f64,
    start_location: Point<i32, Logical>,
    scale: f64,
}

impl DragAndDrop {
    pub fn new(
        canvas: &Canvas,
        overview: &Overview,
        layout_position: LayoutPosition,
        window: Rc<RefCell<Window>>,
    ) -> Self {
        // Calculate X offset when one of the outside windows is being dragged.
        let window_x_offset =
            -(layout_position.index as f64) + (overview.x_offset - overview.x_offset.round());

        // Calculate layout position in overview.
        let available = canvas.available_overview();
        let position = OverviewPosition::new(available, overview.x_offset, window_x_offset);

        // Calculate original position of dragged window.
        let mut start_location = if layout_position.secondary {
            position.secondary_bounds().loc
        } else {
            position.bounds.loc
        };
        start_location += window.borrow().internal_offset().scale(position.scale);

        Self {
            start_location,
            window,
            touch_position: overview.last_drag_point,
            overview_x_offset: overview.x_offset,
            scale: position.scale,
            window_position: Default::default(),
        }
    }

    /// Get textures for rendering the Drag & Drop view.
    pub fn textures(
        &self,
        textures: &mut Vec<CatacombElement>,
        canvas: &Canvas,
        graphics: &Graphics,
    ) {
        let scale = canvas.scale();
        let available = canvas.available_overview().to_f64().to_physical(scale);

        // Get bounds of the drop areas.
        let (primary_bounds, secondary_bounds) = self.drop_bounds(canvas);

        // Render the drop areas.
        let area_scale = available.size.w.max(available.size.h);
        for bounds in [primary_bounds, secondary_bounds] {
            // Get correctly colored texture for the drop area.
            let texture = if bounds.to_f64().contains(self.touch_position) {
                graphics.active_drop_target.clone()
            } else {
                graphics.drop_target.clone()
            };

            // Rescale and crop texture to the desired dimensions.
            let bounds = bounds.to_physical_precise_round(scale);
            CatacombElement::add_element(textures, texture, bounds.loc, bounds, area_scale, scale);
        }

        // Offset by dragged distance.
        let location = self.start_location + self.window_position.to_i32_round();

        // Render the window being drag-and-dropped.
        self.window.borrow().textures(textures, scale, self.scale, location);
    }

    /// Bounds for the drop preview areas of the D&D action.
    pub fn drop_bounds(
        &self,
        canvas: &Canvas,
    ) -> (Rectangle<i32, Logical>, Rectangle<i32, Logical>) {
        let available = canvas.available_overview();
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

/// Purpose of an overview touch drag action.
#[derive(Default, Debug)]
pub enum DragAction {
    /// Close a window in the overview.
    Close(Weak<RefCell<Window>>),
    /// Cycle through overview windows.
    Cycle,
    /// No action active.
    #[default]
    None,
}

impl DragAction {
    /// Window in the process of being closed.
    pub fn closing_window(&self) -> Weak<RefCell<Window>> {
        match self {
            Self::Close(window) => window.clone(),
            _ => Weak::new(),
        }
    }
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
        window_offset: f64,
    ) -> Self {
        // Calculate window's distance from the center of the overview.
        let delta = center_offset_x - window_offset.round();

        // Calculate the window's scale.
        let scale = BG_OVERVIEW_PERCENTAGE
            + (FG_OVERVIEW_PERCENTAGE - BG_OVERVIEW_PERCENTAGE) * (1. - delta.abs());

        // Calculate the window's size and position.
        let bounds_size = available_rect.size.scale(scale);
        let bounds_loc = available_rect.loc + available_rect.size.sub(bounds_size).scale(0.5);
        let mut bounds = Rectangle::from_loc_and_size(bounds_loc, bounds_size);
        bounds.loc.x += (delta * available_rect.size.w as f64 * OVERVIEW_SPACING_PERCENTAGE) as i32;

        Self { bounds, scale }
    }

    /// Secondary window bounds for this layout's position.
    fn secondary_bounds(&self) -> Rectangle<i32, Logical> {
        let secondary_offset = self.bounds.size.h / 2;

        let mut secondary_bounds = self.bounds;
        secondary_bounds.loc.y += secondary_offset;
        secondary_bounds.size.h -= secondary_offset;
        secondary_bounds
    }
}
