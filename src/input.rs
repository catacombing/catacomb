//! Input event handling.

use std::time::{Duration, Instant};

use smithay::backend::input::{
    Event, InputBackend, InputEvent, KeyState, KeyboardKeyEvent, MouseButton, PointerButtonEvent,
    PointerMotionAbsoluteEvent,
};
use smithay::backend::winit::WinitEvent;
use smithay::utils::{Logical, Point};
use smithay::wayland::seat::{keysyms, FilterResult};
use smithay::wayland::SERIAL_COUNTER;

use crate::catacomb::Catacomb;
use crate::output::Orientation;

/// Mouse pointer sensitivity in the application overview.
const MOUSE_OVERVIEW_SENSITIVITY: f64 = 250.;

/// Maximum time before touch input is considered a drag.
const MAX_TAP_DURATION: Duration = Duration::from_millis(750);

/// Maximum distance before touch input is considered a drag.
const MAX_TAP_DISTANCE: f64 = 35.;

/// Touch input state.
#[derive(Default)]
pub struct TouchState {
    last_drag_end: Option<Point<f64, Logical>>,
    touch_start: Option<TouchStart>,
    position: Point<f64, Logical>,
}

struct TouchStart {
    position: Point<f64, Logical>,
    instant: Instant,
}

impl TouchStart {
    fn new(position: Point<f64, Logical>) -> Self {
        Self { instant: Instant::now(), position }
    }

    /// Determine if a touch release is within margin of error for a single tap.
    fn is_tap(&self, position: Point<f64, Logical>) -> bool {
        let delta = self.position - position;
        self.instant.elapsed() <= MAX_TAP_DURATION
            && f64::sqrt(delta.x.powi(2) + delta.y.powi(2)) <= MAX_TAP_DISTANCE
    }
}

impl Catacomb {
    /// Process winit-specific input events.
    pub fn handle_winit_input(&mut self, event: WinitEvent) {
        match event {
            // Toggle between portrait/landscape based on window size.
            WinitEvent::Resized { size, .. } => {
                if size.h >= size.w {
                    self.output.orientation = Orientation::Portrait;
                } else {
                    self.output.orientation = Orientation::Landscape;
                }
                self.output.mode.size = size;
                self.windows.borrow_mut().start_transaction().update_dimensions(&self.output);
            },
            WinitEvent::Input(event) => self.handle_input(event),
            _ => (),
        }
    }

    /// Process new input events.
    fn handle_input<B: InputBackend>(&mut self, event: InputEvent<B>) {
        match event {
            InputEvent::Keyboard { event, .. } => self.handle_keyboard_input(event),
            InputEvent::PointerButton { event } if event.button() == Some(MouseButton::Left) => {
                if let Some(touch_start) = &self.touch_state.touch_start {
                    if touch_start.is_tap(self.touch_state.position) {
                        self.windows.borrow_mut().on_tap(&self.output, self.touch_state.position);
                    } else {
                        self.windows.borrow_mut().on_drag_release();
                    }
                }
                self.touch_state.touch_start = Some(TouchStart::new(self.touch_state.position))
                    .xor(self.touch_state.touch_start.take());

                self.touch_state.last_drag_end = None;
            },
            InputEvent::PointerMotionAbsolute { event } => {
                let new_position = event.position_transformed(self.output.size());

                if let Some(touch_start) = self
                    .touch_state
                    .touch_start
                    .as_ref()
                    .filter(|touch_start| !touch_start.is_tap(self.touch_state.position))
                {
                    let position = self.touch_state.last_drag_end.unwrap_or(touch_start.position);
                    let delta = new_position - position;
                    self.windows.borrow_mut().on_drag(delta.x / MOUSE_OVERVIEW_SENSITIVITY);
                    self.touch_state.last_drag_end = Some(new_position);
                }

                self.touch_state.position = new_position;
            },
            _ => (),
        };
    }

    /// Handle new keyboard input events.
    fn handle_keyboard_input<B: InputBackend>(&mut self, event: impl KeyboardKeyEvent<B>) {
        let serial = SERIAL_COUNTER.next_serial();
        let time = Event::time(&event);
        let keycode = event.key_code();
        let state = event.state();

        self.keyboard.input(keycode, state, serial, time, |modifiers, keysym| {
            if modifiers.ctrl && keysym.modified_sym() == keysyms::KEY_t {
                if state == KeyState::Released {
                    self.windows.borrow_mut().toggle_view();
                }
                FilterResult::Intercept(())
            } else {
                FilterResult::Forward
            }
        });
    }
}
