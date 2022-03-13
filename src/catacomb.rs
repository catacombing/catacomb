//! Catacomb compositor state.

use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;
use std::{env, io};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::{EventLoop, Interest, Mode as TriggerMode, PostAction};
use smithay::reexports::wayland_server::Display;
use smithay::utils::Rectangle;
use smithay::wayland::output::xdg;
use smithay::wayland::seat::{KeyboardHandle, Seat, XkbConfig};
use smithay::wayland::shell::legacy::decoration as kde_decoration;
use smithay::wayland::shell::legacy::decoration::KdeDecorationRequest;
use smithay::wayland::shell::xdg::decoration;
use smithay::wayland::{data_device, shm};
use wayland_protocols::misc::server_decoration::server::org_kde_kwin_server_decoration_manager::Mode;

use crate::drawing::Graphics;
use crate::input::TouchState;
use crate::orientation::{Accelerometer, AccelerometerSource};
use crate::output::Output;
use crate::shell;
use crate::window::Windows;

/// Shared compositor state.
pub struct Catacomb<B> {
    pub keyboard: KeyboardHandle,
    pub touch_state: TouchState,
    pub seat_name: String,
    pub terminated: bool,
    pub windows: Windows,
    pub output: Output,
    pub backend: B,

    graphics: Graphics,
    touch_debug: bool,

    // NOTE: Must be last field to ensure it's dropped after any global.
    pub display: Rc<RefCell<Display>>,
}

impl<B: Backend + 'static> Catacomb<B> {
    /// Initialize the compositor.
    pub fn new(event_loop: &mut EventLoop<Self>, backend: B) -> Self {
        let mut display = Display::new();

        // Create our Wayland socket.
        let socket_name = &mut display
            .add_socket_auto()
            .expect("wayland socket")
            .into_string()
            .expect("wayland socket name");
        env::set_var("WAYLAND_DISPLAY", &socket_name);
        println!("Wayland socket: {socket_name}");

        // Subscribe to Wayland socket events.
        event_loop
            .handle()
            .insert_source(
                Generic::from_fd(display.get_poll_fd(), Interest::READ, TriggerMode::Level),
                |_, _, catacomb| catacomb.handle_socket_readiness(),
            )
            .expect("register wayland socket source");

        // Initialize all available shells.
        shell::init::<B>(&mut display);

        // Advertise support for rendering from CPU-based shared memory buffers.
        shm::init_shm_global(&mut display, Vec::new(), None);

        // Force server-side decorations.
        decoration::init_xdg_decoration_manager(&mut display, |_, _| {}, None);
        kde_decoration::init_kde_decoration_manager(
            &mut display,
            |request| match request {
                KdeDecorationRequest::Setup { manager } => manager.default_mode(Mode::Server),
                KdeDecorationRequest::RequestMode { decoration, mode, .. } => decoration.mode(mode),
                _ => (),
            },
            None,
        );

        // Initialize input.
        let seat_name = backend.seat_name();
        let (mut seat, _) = Seat::new(&mut display, seat_name.clone(), None);
        data_device::init_data_device(
            &mut display,
            |_| {},
            data_device::default_action_chooser,
            None,
        );
        let keyboard = seat
            .add_keyboard(XkbConfig::default(), 200, 25, |seat, focused_surface| {
                data_device::set_data_device_focus(
                    seat,
                    focused_surface.and_then(|surface| surface.as_ref().client()),
                )
            })
            .expect("adding keyboard");
        let touch = seat.add_touch();

        // Subscribe to device orientation changes.
        Accelerometer::new().subscribe(event_loop.handle(), |orientation, catacomb| {
            catacomb.handle_orientation(orientation);
        });

        // XDG output protocol.
        xdg::init_xdg_output_manager(&mut display, None);

        Self {
            touch_state: TouchState::new(event_loop.handle(), touch),
            output: Output::new_dummy(&mut display),
            display: Rc::new(RefCell::new(display)),
            windows: Windows::new(),
            seat_name,
            keyboard,
            backend,
            touch_debug: Default::default(),
            terminated: Default::default(),
            graphics: Default::default(),
        }
    }

    /// Handle Wayland event socket read readiness.
    fn handle_socket_readiness(&mut self) -> io::Result<PostAction> {
        let display = self.display.clone();
        let mut display = display.borrow_mut();
        match display.dispatch(Duration::from_millis(0), self) {
            Ok(_) => Ok(PostAction::Continue),
            Err(error) => {
                eprintln!("I/O error on the Wayland display: {error}");
                self.terminated = true;
                Err(error)
            },
        }
    }

    /// Render the current compositor state.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame) {
        // Render debug indicator showing current touch location.
        if self.touch_debug {
            let loc = self.touch_state.position.to_i32_round();
            let touch_debug = self.graphics.touch_debug(renderer);
            let rect = Rectangle::from_loc_and_size(loc, (i32::MAX, i32::MAX));
            touch_debug.draw_at(frame, &self.output, rect, 1.);
        }

        self.windows.draw(renderer, frame, &mut self.graphics, &self.output);
    }
}

pub trait Backend {
    fn seat_name(&self) -> String;
    fn change_vt(&mut self, _vt: i32) {}
}
