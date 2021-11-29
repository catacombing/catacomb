//! Input event handling.

use smithay::backend::input::{
    ButtonState, Event, InputBackend, InputEvent, KeyState, KeyboardKeyEvent, MouseButton,
    PointerButtonEvent, PointerMotionAbsoluteEvent,
};
use smithay::backend::winit::WinitEvent;
use smithay::utils::{Logical, Point};
use smithay::wayland::seat::{keysyms, FilterResult};
use smithay::wayland::SERIAL_COUNTER;

use crate::catacomb::Catacomb;
use crate::output::Orientation;
use crate::window::View;

/// Mouse pointer sensitivity in the application overview.
const POINTER_OVERVIEW_SENSITIVITY: f64 = 250.;

/// Touch input state.
#[derive(Default)]
pub struct TouchState {
    position: Option<Point<f64, Logical>>,
    touching: bool,
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
                self.windows.borrow_mut().update_dimensions(&self.output);
            },
            WinitEvent::Input(event) => self.handle_input(event),
            _ => (),
        }
    }

    /// Process new input events.
    fn handle_input<B: InputBackend>(&mut self, event: InputEvent<B>) {
        match event {
            InputEvent::Keyboard { event, .. } => self.handle_keyboard_input(event),
            InputEvent::PointerButton { event } => {
                if event.button() == Some(MouseButton::Left) {
                    self.touch_state.touching = event.state() == ButtonState::Pressed;
                    if !self.touch_state.touching {
                        self.touch_state.position = None;
                    }
                }
            },
            InputEvent::PointerMotionAbsolute { event } if self.touch_state.touching => {
                let new_position = event.position_transformed(self.output.size());
                let old_position = self.touch_state.position.replace(new_position);
                let delta = old_position.map(|pos| new_position - pos).unwrap_or_default();

                if let View::Overview(offset) = &mut self.windows.borrow_mut().view {
                    *offset += delta.x / POINTER_OVERVIEW_SENSITIVITY;
                }
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
                    match &mut self.windows.borrow_mut().view {
                        view @ View::Overview(_) => *view = View::Workspace,
                        view @ View::Workspace => *view = View::Overview(0.),
                    }
                }
                FilterResult::Intercept(())
            } else {
                FilterResult::Forward
            }
        });
    }
}
