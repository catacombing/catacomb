//! Input event handling.

use std::mem;

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

/// Maximum distance before touch input is considered a drag.
const MAX_TAP_DISTANCE: f64 = 20.;

/// Touch input state.
#[derive(Default)]
pub struct TouchState {
    position: Point<f64, Logical>,
    action: Option<TouchAction>,
}

impl TouchState {
    /// Get the updated active touch action.
    fn action(&mut self) -> &mut Option<TouchAction> {
        // Change state to drag when tap deadzone tolerance was exceeded.
        if let Some(TouchAction::Tap { position }) = self.action {
            let delta = position - self.position;
            if f64::sqrt(delta.x.powi(2) + delta.y.powi(2)) > MAX_TAP_DISTANCE {
                let direction = if delta.x.abs() >= delta.y.abs() {
                    Direction::Horizontal
                } else {
                    Direction::Vertical
                };

                self.action = Some(TouchAction::Drag { last_position: position, direction });
            }
        }

        &mut self.action
    }
}

/// Available touch input actions.
#[derive(Copy, Clone)]
enum TouchAction {
    Tap { position: Point<f64, Logical> },
    Drag { last_position: Point<f64, Logical>, direction: Direction },
}

impl TouchAction {
    fn new(position: Point<f64, Logical>) -> Self {
        TouchAction::Tap { position }
    }
}

/// Directional plane.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Direction {
    Horizontal,
    Vertical,
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
                let mut windows = self.windows.borrow_mut();
                self.touch_state.action = match &self.touch_state.action {
                    Some(TouchAction::Tap { position, .. }) => {
                        windows.on_tap(&self.output, *position);
                        None
                    },
                    Some(TouchAction::Drag { .. }) => {
                        windows.on_drag_release();
                        None
                    },
                    None => {
                        windows.on_touch_start(&self.output, self.touch_state.position);
                        Some(TouchAction::new(self.touch_state.position))
                    },
                };
            },
            InputEvent::PointerMotionAbsolute { event } => {
                let position = event.position_transformed(self.output.size());

                if let Some(TouchAction::Drag { last_position, direction }) =
                    self.touch_state.action()
                {
                    let delta = position - mem::replace(last_position, position);
                    match direction {
                        Direction::Horizontal => {
                            self.windows.borrow_mut().on_horizontal_drag(delta.x);
                        },
                        Direction::Vertical => {
                            self.windows.borrow_mut().on_vertical_drag(&self.output, delta.y);
                        },
                    }
                }

                self.touch_state.position = position;
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
