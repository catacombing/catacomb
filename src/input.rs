//! Input event handling.

use smithay::backend::input::{Event, InputBackend, InputEvent, KeyboardKeyEvent};
use smithay::wayland::seat::FilterResult;
use smithay::wayland::SERIAL_COUNTER;

use crate::catacomb::Catacomb;

impl Catacomb {
    /// Process new events.
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
