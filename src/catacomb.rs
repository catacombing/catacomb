//! Catacomb compositor state.

use std::cell::RefCell;
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{env, io};

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::{EventLoop, Interest, Mode as TriggerMode, PostAction};
use smithay::reexports::wayland_server::Display;
use smithay::wayland::seat::{KeyboardHandle, Seat, XkbConfig};
use smithay::wayland::shell::xdg::decoration::{self, XdgDecorationRequest};
use smithay::wayland::{data_device, shm};
use smithay::reexports::wayland_protocols::unstable::xdg_decoration::v1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;

use crate::output::Output;
use crate::shell::{Shells, Window};

/// Shared compositor state.
pub struct Catacomb {
    pub windows: Rc<RefCell<Vec<Window>>>,
    pub display: Rc<RefCell<Display>>,
    pub keyboard: KeyboardHandle,
    pub start_time: Instant,
    pub terminated: bool,
    pub output: Output,
}

impl Catacomb {
    /// Initialize the compositor.
    pub fn new(mut display: Display, output: Output, event_loop: &mut EventLoop<Self>) -> Self {
        // Create our Wayland socket.
        let socket_name = &mut display
            .add_socket_auto()
            .expect("wayland socket")
            .into_string()
            .expect("wayland socket name");
        env::set_var("WAYLAND_DISPLAY", &socket_name);
        println!("Wayland socket: {}", socket_name);

        // Subscribe to Wayland socket events.
        event_loop
            .handle()
            .insert_source(
                Generic::from_fd(display.get_poll_fd(), Interest::READ, TriggerMode::Level),
                |_, _, catacomb| catacomb.handle_socket_readiness(),
            )
            .expect("register wayland socket source");

        // Advertise support for rendering from CPU-based shared memory buffers.
        shm::init_shm_global(&mut display, Vec::new(), None);

        // Force server-side decorations.
        decoration::init_xdg_decoration_manager(
            &mut display,
            |request, _| match request {
                XdgDecorationRequest::NewToplevelDecoration { toplevel } => {
                    let result = toplevel.with_pending_state(|state| {
                        state.decoration_mode = Some(DecorationMode::ServerSide);
                    });

                    if result.is_ok() {
                        toplevel.send_configure();
                    }
                },
                XdgDecorationRequest::SetMode { .. } => (),
                XdgDecorationRequest::UnsetMode { .. } => (),
            },
            None,
        );

        // Initialize input.
        let (mut seat, _) = Seat::new(&mut display, String::from("seat-0"), None);
        let keyboard = seat
            .add_keyboard(XkbConfig::default(), 200, 25, |seat, focused_surface| {
                data_device::set_data_device_focus(
                    seat,
                    focused_surface.and_then(|surface| surface.as_ref().client()),
                )
            })
            .expect("adding keyboard");

        // Initialize all available shells.
        let shells = Shells::new(&mut display);

        Self {
            display: Rc::new(RefCell::new(display)),
            start_time: Instant::now(),
            windows: shells.windows,
            terminated: false,
            keyboard,
            output,
        }
    }

    /// Handle Wayland event socket read readiness.
    fn handle_socket_readiness(&mut self) -> io::Result<PostAction> {
        let display = self.display.clone();
        let mut display = display.borrow_mut();
        match display.dispatch(Duration::from_millis(0), self) {
            Ok(_) => Ok(PostAction::Continue),
            Err(e) => {
                eprintln!("I/O error on the Wayland display: {}", e);
                self.terminated = true;
                Err(e)
            },
        }
    }

    /// Request redraws for all windows.
    pub fn request_frames(&mut self) {
        let runtime = self.start_time.elapsed().as_millis() as u32;
        for window in self.windows.borrow().iter() {
            window.request_frame(runtime);
        }

        let display = self.display.clone();
        display.borrow_mut().flush_clients(self);
    }

    /// Render all windows.
    pub fn draw_windows(&self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame) {
        for window in self.windows.borrow().iter() {
            window.draw(renderer, frame, self.output.scale);
        }
    }
}
