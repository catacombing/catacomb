//! Catacomb compositor state.

use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use std::time::Duration;
use std::{env, io, mem};

use server_decoration::server::org_kde_kwin_server_decoration_manager::Mode;
use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::Frame;
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::{EventLoop, Interest, Mode as TriggerMode, PostAction};
use smithay::reexports::wayland_protocols::misc::server_decoration;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::Display;
use smithay::utils::{Physical, Rectangle};
use smithay::wayland::input_method::{InputMethodHandle, InputMethodSeatTrait};
use smithay::wayland::output::xdg;
use smithay::wayland::seat::{KeyboardHandle, Seat, XkbConfig};
use smithay::wayland::shell::legacy::decoration as kde_decoration;
use smithay::wayland::shell::legacy::decoration::KdeDecorationRequest;
use smithay::wayland::shell::xdg::decoration;
use smithay::wayland::text_input::{TextInputHandle, TextInputSeatTrait};
use smithay::wayland::virtual_keyboard::VirtualKeyboardHandle;
use smithay::wayland::{data_device, input_method, shm, text_input, SERIAL_COUNTER};

use crate::drawing::{Graphics, MAX_DAMAGE_AGE};
use crate::input::TouchState;
use crate::orientation::{Accelerometer, AccelerometerSource};
use crate::output::Output;
use crate::window::Windows;
use crate::{daemon, shell};

/// The script to run after compositor start.
const POST_START_SCRIPT: &str = "post_start.sh";

/// Shared compositor state.
pub struct Catacomb<B> {
    pub virtual_keyboard: VirtualKeyboardHandle,
    pub input_method: InputMethodHandle,
    pub text_input: TextInputHandle,
    pub keyboard: KeyboardHandle,
    pub touch_state: TouchState,
    pub seat_name: String,
    pub terminated: bool,
    pub windows: Windows,
    pub output: Output,
    pub backend: B,

    last_focus: Option<WlSurface>,
    graphics: Graphics,
    touch_debug: bool,
    damage: Damage,

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

        // Initialize IME and virtual keyboard.
        input_method::init_input_method_manager_global(&mut display);
        text_input::init_text_input_manager_global(&mut display);
        let input_method = seat.add_input_method(25, 200, XkbConfig::default());
        let text_input = seat.add_text_input();
        let virtual_keyboard = VirtualKeyboardHandle::new(&mut display, 200, 25);

        // Subscribe to device orientation changes.
        Accelerometer::new().subscribe(event_loop.handle(), |orientation, catacomb| {
            catacomb.handle_orientation(orientation);
        });

        // XDG output protocol.
        xdg::init_xdg_output_manager(&mut display, None);

        // Run user startup script.
        if let Some(mut script_path) = dirs::config_dir() {
            script_path.push("catacomb");
            script_path.push(POST_START_SCRIPT);
            let _ = daemon::spawn(script_path.as_os_str(), []);
        }

        Self {
            touch_state: TouchState::new(event_loop.handle(), touch),
            output: Output::new_dummy(&mut display),
            display: Rc::new(RefCell::new(display)),
            windows: Windows::new(),
            virtual_keyboard,
            input_method,
            text_input,
            seat_name,
            keyboard,
            backend,
            touch_debug: Default::default(),
            last_focus: Default::default(),
            terminated: Default::default(),
            graphics: Default::default(),
            damage: Default::default(),
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
}

impl<B> Catacomb<B> {
    /// Handle everything necessary to draw a single frame.
    pub fn create_frame<R: Render>(&mut self, mut renderer: R) {
        // Update transaction before rendering to update device orientation.
        self.windows.update_transaction();

        // Update surface focus.
        let focus = self.windows.focus();
        if focus != self.last_focus {
            self.last_focus = focus.clone();
            self.focus(focus.as_ref());
        }

        // Redraw only when there is damage present.
        if self.windows.damaged() || self.touch_debug {
            let _ = renderer.render(self, Catacomb::draw);
        } else {
            renderer.reschedule();
        }

        // Handle window liveliness changes.
        self.windows.refresh(&mut self.output);

        // Request new frames for visible windows.
        self.windows.request_frames();
    }

    /// Draw the current compositor state.
    pub fn draw(&mut self, renderer: &mut Gles2Renderer, frame: &mut Gles2Frame, buffer_age: u8) {
        // Collect pending damage.
        let max_age = MAX_DAMAGE_AGE as u8;
        let damage = if buffer_age == 0
            || buffer_age > max_age
            || self.windows.fully_damaged()
            || self.touch_debug
        {
            let output_size = self.output.size().to_f64().to_physical(self.output.scale());
            self.damage.push(Rectangle::from_loc_and_size((0., 0.), output_size));
            self.damage.take_since(1)
        } else {
            self.windows.window_damage(&mut self.damage);
            self.damage.take_since(buffer_age)
        };

        // Clear the screen.
        let _ = frame.clear([1., 0., 1., 1.], damage);

        // Render debug indicator showing current touch location.
        if self.touch_debug {
            let loc = self.touch_state.position.to_i32_round();
            let touch_debug = self.graphics.touch_debug(renderer);
            let rect = Rectangle::from_loc_and_size(loc, (i32::MAX, i32::MAX));
            touch_debug.draw_at(frame, &self.output, rect, 1., None);
        }

        self.windows.draw(renderer, frame, &mut self.graphics, &self.output, damage);
    }

    /// Focus a new surface.
    pub fn focus(&mut self, surface: Option<&WlSurface>) {
        self.virtual_keyboard.set_focus(surface, SERIAL_COUNTER.next_serial());
        self.keyboard.set_focus(surface, SERIAL_COUNTER.next_serial());
        self.text_input.set_focus(surface, None);
    }
}

/// Backend capabilities.
pub trait Backend {
    fn seat_name(&self) -> String;
    fn change_vt(&mut self, _vt: i32) {}
}

/// Abstraction over backend-specific rendering.
pub trait Render {
    /// Render the frame, using the provided drawing function.
    fn render<B, F>(
        &mut self,
        catacomb: &mut Catacomb<B>,
        draw_fun: F,
    ) -> Result<(), Box<dyn Error>>
    where
        F: FnOnce(&mut Catacomb<B>, &mut Gles2Renderer, &mut Gles2Frame, u8);

    /// Re-schedule the rendering.
    ///
    /// Re-rendering at a later point will be requested when the current frame
    /// completed without any damage present.
    fn reschedule(&mut self) {}
}

#[derive(Default, Debug)]
pub struct Damage {
    /// Combined damage history for all tracked buffer ages.
    damage: Vec<Rectangle<f64, Physical>>,

    /// Tracked damage rectangles per buffer age.
    ///
    /// This must be one bigger than [`MAX_DAMAGE_AGE`], since the last slot is
    /// reserved for pending damage for the next frame.
    rects: [usize; MAX_DAMAGE_AGE + 1],

    /// Buffer for storing current damage.
    ///
    /// This is used to deduplicate the damage rectangles to prevent excessive
    /// draw calls.
    current: Vec<Rectangle<f64, Physical>>,
}

impl Damage {
    /// Add pending damage for the next frame.
    pub fn push(&mut self, damage: Rectangle<f64, Physical>) {
        self.rects[self.rects.len() - 1] += 1;
        self.damage.push(damage);
    }

    /// Calculate damage history since buffer age.
    ///
    /// This will also clear the pending damage, pushing it into history and
    /// preparing the storage for new damage.
    fn take_since(&mut self, buffer_age: u8) -> &[Rectangle<f64, Physical>] {
        // Move pending damage into history.

        let oldest_rects = mem::take(&mut self.rects[self.rects.len() - 2]);
        let new_rects = self.rects[self.rects.len() - 1];
        self.rects.rotate_right(1);

        self.damage.rotate_right(new_rects);
        self.damage.truncate(self.damage.len() - oldest_rects);

        // Compute rects relevant for current buffer age.
        let rects = self.rects.iter().take(buffer_age as usize).sum();

        // Optimize damage rectangle count.
        self.current.clear();
        for damage in &self.damage[..rects] {
            // Combine overlapping rects.
            let overlap = self.current.iter_mut().find(|staged| staged.overlaps(*damage));
            match overlap {
                Some(overlap) => *overlap = overlap.merge(*damage),
                None => self.current.push(*damage),
            }
        }

        self.current.as_slice()
    }
}
