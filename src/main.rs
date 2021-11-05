use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

use smithay::backend::renderer::{Frame, ImportDma, ImportEgl};
use smithay::backend::winit;
use smithay::reexports::calloop::EventLoop;
use smithay::reexports::wayland_server::Display;
use smithay::wayland::dmabuf;
use smithay::wayland::output::Mode;

use crate::catacomb::Catacomb;
use crate::output::Output;

mod catacomb;
mod input;
mod output;
mod shell;
mod window;

fn main() {
    let mut display = Display::new();

    let (graphics, mut input) = winit::init(None).expect("init winit");
    let graphics = Rc::new(RefCell::new(graphics));

    // Setup hardware acceleration.
    if graphics.borrow_mut().renderer().bind_wl_display(&display).is_ok() {
        let formats: Vec<_> = graphics.borrow_mut().renderer().dmabuf_formats().cloned().collect();
        let graphics = graphics.clone();
        dmabuf::init_dmabuf_global(
            &mut display,
            formats,
            move |buffer, _| graphics.borrow_mut().renderer().import_dmabuf(buffer).is_ok(),
            None,
        );
    }

    let mode = Mode { size: graphics.borrow().window_size().physical_size, refresh: 200_000 };
    let output = Output::new(mode);

    let mut event_loop: EventLoop<'_, Catacomb> = EventLoop::try_new().expect("event loop");
    let mut catacomb = Catacomb::new(display, output, &mut event_loop);

    let display = catacomb.display.clone();
    loop {
        if input.dispatch_new_events(|event| catacomb.handle_winit_input(event)).is_err() {
            eprintln!("input error");
            break;
        }

        // Attempt to process staged atomic updates.
        catacomb.windows.borrow_mut().update_transaction();

        graphics
            .borrow_mut()
            .render(|renderer, frame| {
                let _ = frame.clear([1., 0., 1., 1.]);

                catacomb.draw_windows(renderer, frame);
            })
            .expect("buffer swap");

        // Handle window liveliness changes.
        catacomb.windows.borrow_mut().refresh(&catacomb.output);

        catacomb.request_frames();

        // NOTE: The timeout picked here is 5ms to allow for up to 200 FPS. Increasing it would
        // reduce the framerate, while decreasing it would mean that most of the vblank interval is
        // spent not doing anything, rather than handling events.
        if event_loop.dispatch(Some(Duration::from_millis(5)), &mut catacomb).is_err() {
            eprintln!("event loop error");
            break;
        }

        display.borrow_mut().flush_clients(&mut catacomb);
    }
}
