//! Catacomb compositor state.

use std::cell::RefCell;
use std::env;
use std::os::unix::io::AsRawFd;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use _decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;
use _server_decoration::server::org_kde_kwin_server_decoration_manager::Mode as ManagerMode;
use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::renderer::ImportDma;
use smithay::input::keyboard::XkbConfig;
use smithay::input::{Seat, SeatHandler, SeatState};
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::signals::{Signal, Signals};
use smithay::reexports::calloop::{
    Interest, LoopHandle, Mode as TriggerMode, PostAction, RegistrationToken,
};
use smithay::reexports::wayland_protocols::xdg::decoration as _decoration;
use smithay::reexports::wayland_protocols_misc::server_decoration as _server_decoration;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;
use smithay::reexports::wayland_server::protocol::wl_seat::WlSeat;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{Display, DisplayHandle, Resource};
use smithay::utils::{Serial, SERIAL_COUNTER};
use smithay::wayland::buffer::BufferHandler;
use smithay::wayland::compositor::{CompositorHandler, CompositorState};
use smithay::wayland::data_device::{
    ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
};
use smithay::wayland::dmabuf::{DmabufGlobal, DmabufHandler, DmabufState, ImportError};
use smithay::wayland::fractional_scale::{
    self, FractionalScaleHandler, FractionalScaleManagerState,
};
use smithay::wayland::input_method::{InputMethodManagerState, InputMethodSeat};
use smithay::wayland::output::OutputManagerState;
use smithay::wayland::presentation::PresentationState;
use smithay::wayland::shell::kde::decoration::{KdeDecorationHandler, KdeDecorationState};
use smithay::wayland::shell::wlr_layer::{
    Layer, LayerSurface, WlrLayerShellHandler, WlrLayerShellState,
};
use smithay::wayland::shell::xdg::decoration::{XdgDecorationHandler, XdgDecorationState};
use smithay::wayland::shell::xdg::{
    Configure, PopupSurface, PositionerState, ToplevelSurface, XdgShellHandler, XdgShellState,
};
use smithay::wayland::shm::{ShmHandler, ShmState};
use smithay::wayland::socket::ListeningSocketSource;
use smithay::wayland::text_input::{TextInputHandle, TextInputManagerState};
use smithay::wayland::viewporter::ViewporterState;
use smithay::wayland::virtual_keyboard::VirtualKeyboardManagerState;
use smithay::wayland::{compositor, data_device};
use smithay::{
    delegate_compositor, delegate_data_device, delegate_dmabuf, delegate_fractional_scale,
    delegate_input_method_manager, delegate_kde_decoration, delegate_layer_shell, delegate_output,
    delegate_presentation, delegate_seat, delegate_shm, delegate_text_input_manager,
    delegate_viewporter, delegate_virtual_keyboard_manager, delegate_xdg_decoration,
    delegate_xdg_shell,
};

use crate::input::{PhysicalButtonState, TouchState};
use crate::orientation::{Accelerometer, AccelerometerSource};
use crate::output::Output;
use crate::protocols::screencopy::frame::Screencopy;
use crate::protocols::screencopy::{ScreencopyHandler, ScreencopyManagerState};
use crate::udev::Udev;
use crate::vibrate::Vibrator;
use crate::windows::Windows;
use crate::{dbus, delegate_screencopy_manager};

/// The script to run after compositor start.
const POST_START_SCRIPT: &str = "post_start.sh";

/// Shared compositor state.
pub struct Catacomb {
    pub suspend_timer: Option<RegistrationToken>,
    pub event_loop: LoopHandle<'static, Self>,
    pub button_state: PhysicalButtonState,
    pub display_handle: DisplayHandle,
    pub touch_state: TouchState,
    pub last_resume: Instant,
    pub socket_name: String,
    pub vibrator: Vibrator,
    pub seat_name: String,
    pub windows: Windows,
    pub seat: Seat<Self>,
    pub terminated: bool,
    pub sleeping: bool,
    pub backend: Udev,

    // Smithay state.
    pub dmabuf_state: DmabufState,
    kde_decoration_state: KdeDecorationState,
    layer_shell_state: WlrLayerShellState,
    data_device_state: DataDeviceState,
    compositor_state: CompositorState,
    xdg_shell_state: XdgShellState,
    seat_state: SeatState<Self>,
    shm_state: ShmState,

    accelerometer_token: RegistrationToken,
    last_focus: Option<WlSurface>,

    // Indicates if rendering was intentionally stalled.
    //
    // This will occur when rendering on a VBlank detects no damage is present, thus stopping
    // rendering and further VBlank. If this is `true`, rendering needs to be kicked of manually
    // again when damage is received.
    stalled: bool,

    // NOTE: Must be last field to ensure it's dropped after any global.
    pub display: Rc<RefCell<Display<Self>>>,
}

impl Catacomb {
    /// Initialize the compositor.
    pub fn new(event_loop: LoopHandle<'static, Self>, backend: Udev) -> Self {
        let mut display = Display::new().expect("Wayland display creation");
        let display_handle = display.handle();

        // Setup SIGTERM handler for clean shutdown.
        let signals = Signals::new(&[Signal::SIGTERM]).expect("masking unix signals");
        event_loop
            .insert_source(signals, |_, _, catacomb| catacomb.terminated = true)
            .expect("register unix signal source");

        // Create and register Wayland socket.
        let socket_source = ListeningSocketSource::new_auto().expect("create Wayland socket");
        let socket_name = socket_source.socket_name().to_string_lossy().into_owned();
        event_loop
            .insert_source(socket_source, move |stream, _, catacomb| {
                let _ = catacomb.display_handle.insert_client(stream, Arc::new(()));
            })
            .expect("register Wayland socket source");

        // Log and set `WAYLAND_DISPLAY` for children.
        env::set_var("WAYLAND_DISPLAY", &socket_name);
        println!("Wayland socket: {socket_name}");

        // Register display event source.
        event_loop
            .insert_source(
                Generic::new(
                    display.backend().poll_fd().as_raw_fd(),
                    Interest::READ,
                    TriggerMode::Level,
                ),
                |_, _, catacomb| Ok(catacomb.handle_socket_readiness()),
            )
            .expect("register display");

        // Create the compositor and register a surface commit handler.
        let compositor_state = CompositorState::new::<Self>(&display_handle);

        // Setup XDG Shell.
        let xdg_shell_state = XdgShellState::new::<Self>(&display_handle);

        // Setup layer shell.
        let layer_shell_state = WlrLayerShellState::new::<Self>(&display_handle);

        // Advertise support for rendering from CPU-based shared memory buffers.
        let shm_state = ShmState::new::<Self>(&display_handle, Vec::new());

        // Advertise support for rendering GPU-based buffers.
        let dmabuf_state = DmabufState::new();

        // XDG output protocol.
        OutputManagerState::new_with_xdg_output::<Self>(&display_handle);

        // Fractional scale protocol.
        FractionalScaleManagerState::new::<Self>(&display_handle);

        // Initialize wp_viewporter protocol.
        ViewporterState::new::<Self>(&display_handle);

        // Force server-side decorations.
        XdgDecorationState::new::<Self>(&display_handle);
        let kde_decoration_state =
            KdeDecorationState::new::<Self>(&display_handle, ManagerMode::Server);

        // Initialize screencopy protocol.
        ScreencopyManagerState::new::<Self>(&display_handle);

        // Initialize wp_presentation protocol.
        let clock_id = libc::CLOCK_MONOTONIC as u32;
        PresentationState::new::<Self>(&display_handle, clock_id);

        // Initialize seat.
        let seat_name = backend.seat_name();
        let mut seat_state = SeatState::new();
        let mut seat = seat_state.new_wl_seat(&display_handle, seat_name.clone());

        // Initialize IME and virtual keyboard.
        InputMethodManagerState::new::<Self>(&display_handle);
        seat.add_input_method(XkbConfig::default(), 200, 25);
        TextInputManagerState::new::<Self>(&display_handle);
        VirtualKeyboardManagerState::new::<Self, _>(&display_handle, |_| true);

        // Initialize keyboard/touch/data device.
        let data_device_state = DataDeviceState::new::<Self>(&display_handle);
        seat.add_keyboard(XkbConfig::default(), 200, 25).expect("adding keyboard");

        // Initialize touch state.
        let touch = seat.add_touch();
        let touch_state = TouchState::new(event_loop.clone(), touch);

        // Subscribe to device orientation changes.
        let accel_token = Accelerometer::new().subscribe(&event_loop, |orientation, catacomb| {
            catacomb.handle_orientation(orientation);
        });

        // Setup rumble device.
        let vibrator = Vibrator::new(event_loop.clone());

        // Run user startup script.
        if let Some(mut script_path) = dirs::config_dir() {
            script_path.push("catacomb");
            script_path.push(POST_START_SCRIPT);

            if let Err(err) = crate::daemon(script_path.as_os_str(), []) {
                eprintln!("Unable to launch {script_path:?}: {err}");
            }
        }

        // Create window manager.
        let windows = Windows::new(&display_handle, event_loop.clone());

        Self {
            kde_decoration_state,
            layer_shell_state,
            data_device_state,
            compositor_state,
            xdg_shell_state,
            display_handle,
            dmabuf_state,
            touch_state,
            socket_name,
            event_loop,
            seat_state,
            shm_state,
            seat_name,
            vibrator,
            windows,
            backend,
            seat,
            display: Rc::new(RefCell::new(display)),
            accelerometer_token: accel_token,
            last_resume: Instant::now(),
            suspend_timer: Default::default(),
            button_state: Default::default(),
            last_focus: Default::default(),
            terminated: Default::default(),
            sleeping: Default::default(),
            stalled: Default::default(),
        }
    }

    /// Handle Wayland event socket read readiness.
    fn handle_socket_readiness(&mut self) -> PostAction {
        let display = self.display.clone();
        let mut display = display.borrow_mut();
        display.dispatch_clients(self).expect("Wayland dispatch error");
        PostAction::Continue
    }

    /// Handle everything necessary to draw a single frame.
    pub fn create_frame(&mut self) {
        // Clear rendering stall status.
        self.stalled = false;

        // Ensure no redraws are queued beyond this one.
        self.backend.cancel_scheduled_redraws();

        // Update transaction before rendering to update device orientation.
        let transaction_deadline = self.windows.update_transaction();

        // Update surface focus.
        let focus = self.windows.focus();
        if focus != self.last_focus {
            self.last_focus = focus.clone();
            self.focus(focus);
        }

        // Redraw only when there is damage present.
        if self.windows.damaged() {
            // Apply pending client updates.
            if let Some(renderer) = self.backend.renderer() {
                self.windows.import_buffers(renderer);
            }

            // Draw all visible clients.
            let rendered = self.backend.render(&mut self.windows);

            // Create artificial VBlank if renderer didn't draw.
            //
            // This is necessary, since rendering might have been skipped due to DRM planes
            // and the next frame could still contain more damage like overview animations.
            if !rendered {
                let frame_interval = self.windows.output().frame_interval();
                self.backend.schedule_redraw(frame_interval);
            }
        } else if let Some(deadline) = transaction_deadline {
            // Force a redraw after the transaction has timed out.
            self.backend.schedule_redraw(deadline);
            self.stalled = true;
        } else {
            // Indicate rendering was stalled.
            self.stalled = true;
        }

        // Request new frames for visible windows.
        self.windows.request_frames();
    }

    /// Focus a new surface.
    pub fn focus(&mut self, surface: Option<WlSurface>) {
        if let Some(text_input) = self.seat.user_data().get::<TextInputHandle>() {
            text_input.set_focus(surface.as_ref(), || {});
        }
        if let Some(keyboard) = self.seat.get_keyboard() {
            keyboard.set_focus(self, surface, SERIAL_COUNTER.next_serial());
        }
    }

    /// Start rendering again if we're currently stalled.
    pub fn unstall(&mut self) {
        if self.stalled {
            self.create_frame();
        }
    }

    /// Toggle sleep state.
    pub fn toggle_sleep(&mut self) {
        if !self.sleeping {
            self.sleep();
        } else {
            self.resume();
        }
    }

    /// Start active sleep.
    pub fn sleep(&mut self) {
        // Disable accelerometer timer while sleeping.
        let _ = self.event_loop.disable(&self.accelerometer_token);

        self.backend.set_sleep(true);
        self.sleeping = true;
    }

    /// Resume after sleep.
    pub fn resume(&mut self) {
        // Update resume time to ignore buttons pressed during sleep.
        self.last_resume = Instant::now();

        let _ = self.event_loop.enable(&self.accelerometer_token);
        self.backend.set_sleep(false);
        self.sleeping = false;
    }

    /// Suspend to RAM.
    pub fn suspend(&mut self) {
        if let Err(err) = dbus::suspend() {
            eprintln!("Failed suspending to RAM: {err}");
        }
    }
}

impl CompositorHandler for Catacomb {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn commit(&mut self, surface: &WlSurface) {
        if compositor::is_sync_subsurface(surface) {
            return;
        }

        self.windows.surface_commit(surface);

        self.unstall();
    }
}
delegate_compositor!(Catacomb);

impl ShmHandler for Catacomb {
    fn shm_state(&self) -> &ShmState {
        &self.shm_state
    }
}
delegate_shm!(Catacomb);

impl DmabufHandler for Catacomb {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        &mut self.dmabuf_state
    }

    fn dmabuf_imported(
        &mut self,
        _global: &DmabufGlobal,
        buffer: Dmabuf,
    ) -> Result<(), ImportError> {
        let renderer = match self.backend.renderer() {
            Some(renderer) => renderer,
            None => return Err(ImportError::Failed),
        };

        renderer.import_dmabuf(&buffer, None).map_err(|_| ImportError::Failed)?;

        Ok(())
    }
}

delegate_dmabuf!(Catacomb);

impl XdgShellHandler for Catacomb {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        self.windows.add(surface);
    }

    fn new_popup(&mut self, surface: PopupSurface, _positioner: PositionerState) {
        self.windows.add_popup(surface);
    }

    fn ack_configure(&mut self, surface: WlSurface, _configure: Configure) {
        // Request new frames after each resize.
        let runtime = self.windows.runtime();
        if let Some(window) = self.windows.find_xdg(&surface) {
            window.request_frame(runtime);
        }
    }

    fn fullscreen_request(&mut self, surface: ToplevelSurface, _output: Option<WlOutput>) {
        self.windows.fullscreen(&surface);
    }

    fn unfullscreen_request(&mut self, surface: ToplevelSurface) {
        self.windows.unfullscreen(&surface);
    }

    fn toplevel_destroyed(&mut self, surface: ToplevelSurface) {
        self.windows.reap_xdg(&surface);
        self.unstall();
    }

    fn popup_destroyed(&mut self, _surface: PopupSurface) {
        self.windows.refresh_popups();
        self.unstall();
    }

    fn grab(&mut self, _surface: PopupSurface, _seat: WlSeat, _serial: Serial) {}
}
delegate_xdg_shell!(Catacomb);

impl WlrLayerShellHandler for Catacomb {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        surface: LayerSurface,
        _wl_output: Option<WlOutput>,
        layer: Layer,
        _namespace: String,
    ) {
        self.windows.add_layer(layer, surface);
    }

    fn layer_destroyed(&mut self, surface: LayerSurface) {
        self.windows.reap_layer(&surface);
        self.unstall();
    }
}
delegate_layer_shell!(Catacomb);

impl SeatHandler for Catacomb {
    type KeyboardFocus = WlSurface;
    type PointerFocus = WlSurface;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.seat_state
    }

    fn focus_changed(&mut self, seat: &Seat<Self>, surface: Option<&Self::KeyboardFocus>) {
        let client = surface.and_then(|surface| self.display_handle.get_client(surface.id()).ok());
        data_device::set_data_device_focus(&self.display_handle, seat, client);
    }
}
delegate_seat!(Catacomb);

delegate_virtual_keyboard_manager!(Catacomb);
delegate_input_method_manager!(Catacomb);
delegate_text_input_manager!(Catacomb);

impl DataDeviceHandler for Catacomb {
    fn data_device_state(&self) -> &DataDeviceState {
        &self.data_device_state
    }
}
impl ClientDndGrabHandler for Catacomb {}
impl ServerDndGrabHandler for Catacomb {}
delegate_data_device!(Catacomb);

delegate_output!(Catacomb);

impl XdgDecorationHandler for Catacomb {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        toplevel.with_pending_state(|state| {
            state.decoration_mode = Some(DecorationMode::ServerSide);
        });
        toplevel.send_configure();
    }

    fn request_mode(&mut self, _toplevel: ToplevelSurface, _mode: DecorationMode) {}

    fn unset_mode(&mut self, _toplevel: ToplevelSurface) {}
}
delegate_xdg_decoration!(Catacomb);

impl KdeDecorationHandler for Catacomb {
    fn kde_decoration_state(&self) -> &KdeDecorationState {
        &self.kde_decoration_state
    }
}
delegate_kde_decoration!(Catacomb);

impl BufferHandler for Catacomb {
    fn buffer_destroyed(&mut self, _buffer: &WlBuffer) {}
}

impl ScreencopyHandler for Catacomb {
    fn output(&mut self) -> &Output {
        self.windows.output()
    }

    fn frame(&mut self, screencopy: Screencopy) {
        self.backend.request_screencopy(screencopy);

        // Force redraw, to prevent screencopy stalling.
        self.windows.dirty = true;
        self.unstall();
    }
}
delegate_screencopy_manager!(Catacomb);

delegate_presentation!(Catacomb);

impl FractionalScaleHandler for Catacomb {
    fn new_fractional_scale(&mut self, surface: WlSurface) {
        compositor::with_states(&surface, |states| {
            fractional_scale::with_fractional_scale(states, |fractional_scale| {
                let scale = self.windows.output().scale();
                fractional_scale.set_preferred_scale(scale);
            });
        });
    }
}
delegate_fractional_scale!(Catacomb);

delegate_viewporter!(Catacomb);
