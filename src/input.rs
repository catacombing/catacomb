//! Input event handling.

use smithay::backend::input::{Event, InputBackend, InputEvent, KeyboardKeyEvent};
use smithay::backend::winit::WinitEvent;
use smithay::wayland::seat::FilterResult;
use smithay::wayland::SERIAL_COUNTER;

use crate::catacomb::Catacomb;
use crate::output::Orientation;

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
    pub fn handle_input<B: InputBackend>(&self, event: InputEvent<B>) {
        let event = match event {
            InputEvent::Keyboard { event, .. } => event,
            _ => return,
        };

        let serial = SERIAL_COUNTER.next_serial();
        let time = Event::time(&event);
        let keycode = event.key_code();
        let state = event.state();

        self.keyboard.input::<(), _>(keycode, state, serial, time, |_, _| FilterResult::Forward);
    }
}
