//! Application overview.

use std::cell::RefCell;
use std::rc::{Rc, Weak};

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
/// overview at rest.
const PRIMARY_PERCENTAGE: f64 = 0.75;

/// Size percentage the background windows will have in the overview compared to
/// the primary.
const SECONDARY_PERCENTAGE: f64 = 0.95;

/// Percentage of the screen for the drop highlight areas.
const DRAG_AND_DROP_PERCENTAGE: f64 = 0.3;

/// Percentage of the output height a window can be moved before closing it in
/// the overview.
const OVERVIEW_CLOSE_DISTANCE: f64 = 0.25;

/// Maximum amount of overdrag before inputs are ignored.
const OVERDRAG_LIMIT: f64 = 0.25;

/// Primary percentage below which a minimize action will be considered
/// successful.
const MINIMIZE_PERCENTAGE: f64 = 0.1;

/// Overview view state.
#[derive(Debug)]
pub struct Overview {
    pub hold_timer: Option<RegistrationToken>,
    pub last_drag_point: Point<f64, Logical>,
    pub drag_action: DragAction,
    pub primary_percentage: f64,
    pub x_offset: f64,
    pub y_offset: f64,
    pub opened: bool,
    pub dirty: bool,
}

impl Overview {
    pub fn new(active_offset: f64, primary_percentage: Option<f64>) -> Self {
        // Check if opening needs to be animated.
        let (primary_percentage, opened) = match primary_percentage {
            Some(primary_percentage) => (primary_percentage, false),
            None => (PRIMARY_PERCENTAGE, true),
        };

        Self {
            primary_percentage,
            opened,
            x_offset: active_offset,
            last_drag_point: Default::default(),
            drag_action: Default::default(),
            hold_timer: Default::default(),
            y_offset: Default::default(),
            dirty: Default::default(),
        }
    }

    /// Update interactive opening animation.
    pub fn set_open_percentage(&mut self, output: &Output, mut position: f64) {
        // Clamp position to overview bounds.
        let available = output.available_overview().to_f64();
        position = position.clamp(available.loc.y, available.loc.y + available.size.h);

        // Calculate percentage of position from the bottom of the overview.
        let mut delta = 1. - (position - available.loc.y) / available.size.h;

        // Adjust for shrinking both bottom and top.
        delta *= 2.;

        // Calculate desired primary window percentage.
        if self.opened {
            self.primary_percentage = PRIMARY_PERCENTAGE - delta;
        } else if self.primary_percentage < PRIMARY_PERCENTAGE {
            let primary_percentage = delta * PRIMARY_PERCENTAGE / (1. - PRIMARY_PERCENTAGE);
            self.primary_percentage = primary_percentage.min(PRIMARY_PERCENTAGE);
        } else if self.primary_percentage > PRIMARY_PERCENTAGE {
            self.primary_percentage = (1. - delta).max(PRIMARY_PERCENTAGE);
        }

        // Trigger redraw.
        self.dirty = true;
    }

    /// Check if gesture was successful.
    ///
    /// A return value of `false` indicates that the gesture failed and any
    /// state changes should be rolled back.
    pub fn gesture_threshold_passed(&mut self) -> bool {
        let open_done = !self.opened && self.primary_percentage == PRIMARY_PERCENTAGE;
        let minimize_done = self.opened && self.primary_percentage <= MINIMIZE_PERCENTAGE;
        open_done || minimize_done
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
        let scale = canvas.scale();

        let mut offset = self.x_offset - 1.;
        while offset < self.x_offset + 2. {
            let position =
                OverviewPosition::new(available, self.primary_percentage, self.x_offset, offset);
            if position.layout_bounds().to_f64().contains(point) {
                let layout_index = usize::try_from(-offset.round() as isize).ok()?;
                let layout = layouts.get(layout_index)?;

                // Check if window contains the point.
                let contains_point = |window: &Rc<RefCell<Window>>| {
                    let bounds = window.borrow().bounds(scale);
                    position.window_bounds(bounds).to_f64().contains(point)
                };

                // Check if click was within secondary window.
                if layout.secondary().map_or(false, contains_point) {
                    return Some(LayoutPosition::new(layout_index, true));
                }

                // Check if click was within primary window.
                if layout.primary().map_or(false, contains_point) {
                    return Some(LayoutPosition::new(layout_index, false));
                }
            }

            offset += 1.;
        }

        None
    }

    /// Clamp the X/Y offsets.
    fn clamp_offset(&mut self, window_count: i32) {
        // Limit maximum overdrag.
        let min_offset = -window_count as f64 + 1.;
        self.x_offset = self.x_offset.clamp(min_offset - OVERDRAG_LIMIT, OVERDRAG_LIMIT);

        // Horizontal bounce-back to closest valid integer offset.
        if self.drag_action.done {
            self.x_offset = self.x_offset.round().clamp(min_offset, 0.);
        }
    }

    /// Get textures for rendering the overview.
    pub fn textures(
        &mut self,
        textures: &mut Vec<CatacombElement>,
        output: &Output,
        canvas: &Canvas,
        layouts: &Layouts,
    ) {
        // Reset redraw flag.
        self.dirty = false;

        let layout_count = layouts.len() as i32;
        self.clamp_offset(layout_count);

        let available_overview = canvas.available_overview();
        let scale = canvas.scale();

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

            let position = OverviewPosition::new(
                available_overview,
                self.primary_percentage,
                self.x_offset,
                offset,
            );

            // Add secondary window textures.
            if let Some(secondary) = layout.secondary() {
                // Scale window bounds to overview scale.
                let mut bounds = position.window_bounds(secondary.borrow().bounds(scale));

                // Offset bounds for closing windows.
                let closing_offset = self.handle_closing(output, canvas, layouts, secondary, false);
                bounds.loc.y += closing_offset;

                // Get physical bounds for clamping.
                let mut layout_bounds = position.layout_bounds();
                layout_bounds.loc.y += closing_offset;
                let physical_bounds = layout_bounds.to_physical_precise_round(scale);

                // Get window textures within its overview bounds.
                secondary.borrow().textures_with_bounds(
                    textures,
                    scale,
                    position.scale,
                    bounds.loc,
                    physical_bounds,
                );
            }

            // Add primary window textures.
            if let Some(primary) = layout.primary() {
                // Scale window bounds to overview scale.
                let mut bounds = position.window_bounds(primary.borrow().bounds(scale));

                // Offset bounds for closing windows.
                let closing_offset = self.handle_closing(output, canvas, layouts, primary, true);
                bounds.loc.y += closing_offset;

                // Get physical bounds for clamping.
                let mut layout_bounds = position.layout_bounds();
                layout_bounds.loc.y += closing_offset;
                let physical_bounds = layout_bounds.to_physical_precise_round(scale);

                // Get window textures within its overview bounds.
                primary.borrow().textures_with_bounds(
                    textures,
                    scale,
                    position.scale,
                    bounds.loc,
                    physical_bounds,
                );
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
        is_primary: bool,
    ) -> i32 {
        // Check if this window is being closed.
        let closing_window = self.drag_action.closing_window();
        if !Weak::ptr_eq(&closing_window, &Rc::downgrade(window)) {
            return 0;
        }

        // Prevent dragging primary/secondary across each other.
        if is_primary {
            self.y_offset = self.y_offset.min(0.);
        } else {
            self.y_offset = self.y_offset.max(0.);
        }

        // Calculate target window bounds.
        let delta = self.y_offset.round() as i32;

        // Handle close drag termination.
        if self.drag_action.done {
            let close_distance =
                canvas.available_overview().size.h as f64 * OVERVIEW_CLOSE_DISTANCE;

            if self.y_offset.abs() >= close_distance {
                // Kill the window if it is beyond the point of no return.
                let surface = {
                    let mut window = window.borrow_mut();
                    window.kill();
                    window.surface.clone()
                };
                layouts.reap(output, &surface);
            } else {
                // Reset if the window shouldn't be closed.
                self.y_offset = 0.;
            }

            self.dirty = true;
        }

        delta
    }

    /// Check if one of the horizontal scrolling boundaries was reached.
    pub fn cycle_edge_reached(&self, window_count: usize) -> bool {
        let min_offset = -(window_count as f64) + 1.;
        self.x_offset <= min_offset || self.x_offset >= 0.
    }

    /// Check if overview requires a redraw.
    pub fn dirty(&self) -> bool {
        self.dirty
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
        let position = OverviewPosition::new(
            available,
            PRIMARY_PERCENTAGE,
            overview.x_offset,
            window_x_offset,
        );

        // Calculate original position of dragged window.
        let loc = window.borrow().bounds(canvas.scale()).loc;
        let start_location = position.window_loc(loc);

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

/// Overview touch drag state.
#[derive(Default, Debug)]
pub struct DragAction {
    pub action_type: DragActionType,
    pub done: bool,
}

impl DragAction {
    /// Window in the process of being closed.
    pub fn closing_window(&self) -> Weak<RefCell<Window>> {
        match &self.action_type {
            DragActionType::Close(window) => window.clone(),
            _ => Weak::new(),
        }
    }
}

impl From<DragActionType> for DragAction {
    fn from(action_type: DragActionType) -> Self {
        Self { action_type, done: false }
    }
}

/// Purpose of an overview touch drag action.
#[derive(Default, Debug)]
pub enum DragActionType {
    /// Close a window in the overview.
    Close(Weak<RefCell<Window>>),
    /// Cycle through overview windows.
    Cycle,
    /// No action active.
    #[default]
    None,
}

/// Window placed in the application overview.
#[derive(Debug)]
struct OverviewPosition {
    layout_bounds: Rectangle<i32, Logical>,
    available_start: Point<i32, Logical>,
    offset: Point<i32, Logical>,
    scale: f64,
}

impl OverviewPosition {
    /// Calculate the rectangle for a window in the application overview.
    fn new(
        available_rect: Rectangle<i32, Logical>,
        primary_percentage: f64,
        center_offset_x: f64,
        window_offset: f64,
    ) -> Self {
        // Calculate window's distance from the center of the overview.
        let delta = center_offset_x - window_offset.round();

        // Calculate the window's scale.
        let secondary_percentage = primary_percentage * SECONDARY_PERCENTAGE;
        let scale =
            secondary_percentage + (primary_percentage - secondary_percentage) * (1. - delta.abs());

        // Calculate window's overview position.
        let mut offset = available_rect.size.scale((1. - scale) / 2.).to_point();
        offset.x += (delta * available_rect.size.w as f64 * primary_percentage) as i32;

        // Get space at the top-left reserved for exclusive windows in the overview.
        let exclusive_top = available_rect.loc;

        // Calculate maximum layout bounds.
        let mut layout_bounds = available_rect;
        layout_bounds.size = layout_bounds.size.scale(scale);
        layout_bounds.loc += offset;

        Self { layout_bounds, available_start: exclusive_top, offset, scale }
    }

    /// Convert a window's position to its overview position.
    #[must_use]
    fn window_loc(&self, mut loc: Point<i32, Logical>) -> Point<i32, Logical> {
        // Keep offsets from exclusive windows unscaled, while scaling everything else.
        //
        // We explicitly round towards zero when scaling here since a pixel overlap
        // isn't visible, while a pixel gap creates a jarring hole.
        loc -= self.available_start;
        loc.x = (loc.x as f64 * self.scale).trunc() as i32;
        loc.y = (loc.y as f64 * self.scale).trunc() as i32;
        loc += self.available_start;

        // Apply the overview offset.
        loc += self.offset;

        loc
    }

    /// Convert a window's bounds to its overview bounds.
    #[must_use]
    fn window_bounds(&self, mut bounds: Rectangle<i32, Logical>) -> Rectangle<i32, Logical> {
        bounds.loc = self.window_loc(bounds.loc);
        bounds.size = bounds.size.scale(self.scale);
        bounds
    }

    /// Maximum bounds for both windows in this position's layout.
    ///
    /// While most well-behaved clients should always fall within these bounds,
    /// it is necessary to clamp windows to this rectangle to avoid poorly
    /// behaved clients from overlapping other overview positions.
    fn layout_bounds(&self) -> Rectangle<i32, Logical> {
        self.layout_bounds
    }
}
