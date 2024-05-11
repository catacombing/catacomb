//! Catacomb compositor state.

use std::cell::RefCell;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{cmp, env};

use _decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;
use _server_decoration::server::org_kde_kwin_server_decoration_manager::Mode as ManagerMode;
use catacomb_ipc::{Keysym, Orientation};
use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::renderer::ImportDma;
use smithay::input::keyboard::XkbConfig;
use smithay::input::{Seat, SeatHandler, SeatState};
use smithay::reexports::calloop::generic::{Generic, NoIoDrop};
use smithay::reexports::calloop::signals::{Signal, Signals};
use smithay::reexports::calloop::{
    Interest, LoopHandle, Mode as TriggerMode, PostAction, RegistrationToken,
};
use smithay::reexports::wayland_protocols::xdg::decoration as _decoration;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::WmCapabilities;
use smithay::reexports::wayland_protocols_misc::server_decoration as _server_decoration;
use smithay::reexports::wayland_server::backend::{ClientData, ClientId, DisconnectReason};
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;
use smithay::reexports::wayland_server::protocol::wl_seat::WlSeat;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{Client, Display, DisplayHandle, Resource};
use smithay::utils::{Logical, Rectangle, Serial, SERIAL_COUNTER};
use smithay::wayland::buffer::BufferHandler;
use smithay::wayland::compositor;
use smithay::wayland::compositor::{CompositorClientState, CompositorHandler, CompositorState};
use smithay::wayland::dmabuf::{DmabufGlobal, DmabufHandler, DmabufState, ImportNotifier};
use smithay::wayland::fractional_scale::{
    self, FractionalScaleHandler, FractionalScaleManagerState,
};
use smithay::wayland::idle_inhibit::{IdleInhibitHandler, IdleInhibitManagerState};
use smithay::wayland::idle_notify::{IdleNotifierHandler, IdleNotifierState};
use smithay::wayland::input_method::{
    InputMethodHandler, InputMethodManagerState, PopupSurface as ImeSurface,
};
use smithay::wayland::keyboard_shortcuts_inhibit::{
    KeyboardShortcutsInhibitHandler, KeyboardShortcutsInhibitState, KeyboardShortcutsInhibitor,
};
use smithay::wayland::output::{OutputHandler, OutputManagerState};
use smithay::wayland::presentation::PresentationState;
use smithay::wayland::selection::data_device::{
    self, ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
};
use smithay::wayland::selection::primary_selection::{
    self, PrimarySelectionHandler, PrimarySelectionState,
};
use smithay::wayland::selection::wlr_data_control::{DataControlHandler, DataControlState};
use smithay::wayland::selection::SelectionHandler;
use smithay::wayland::session_lock::{
    LockSurface, SessionLockHandler, SessionLockManagerState, SessionLocker,
};
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
use smithay::wayland::text_input::TextInputManagerState;
use smithay::wayland::viewporter::ViewporterState;
use smithay::wayland::virtual_keyboard::VirtualKeyboardManagerState;
use smithay::wayland::xdg_activation::{
    XdgActivationHandler, XdgActivationState, XdgActivationToken, XdgActivationTokenData,
};
use smithay::{
    delegate_compositor, delegate_data_control, delegate_data_device, delegate_dmabuf,
    delegate_fractional_scale, delegate_idle_inhibit, delegate_idle_notify,
    delegate_input_method_manager, delegate_kde_decoration, delegate_keyboard_shortcuts_inhibit,
    delegate_layer_shell, delegate_output, delegate_presentation, delegate_primary_selection,
    delegate_seat, delegate_session_lock, delegate_shm, delegate_text_input_manager,
    delegate_viewporter, delegate_virtual_keyboard_manager, delegate_xdg_activation,
    delegate_xdg_decoration, delegate_xdg_shell,
};
use tracing::{error, info};

use crate::config::KeyBinding;
use crate::drawing::CatacombSurfaceData;
use crate::input::TouchState;
use crate::orientation::{Accelerometer, AccelerometerSource};
use crate::output::Output;
use crate::protocols::screencopy::frame::Screencopy;
use crate::protocols::screencopy::{ScreencopyHandler, ScreencopyManagerState};
use crate::protocols::single_pixel_buffer::SinglePixelBufferState;
use crate::udev::Udev;
use crate::windows::surface::Surface;
use crate::windows::Windows;
use crate::{daemon, delegate_screencopy, delegate_single_pixel_buffer, ipc_server, trace_error};

/// Time before xdg_activation tokens are invalidated.
const ACTIVATION_TIMEOUT: Duration = Duration::from_secs(10);

/// The script to run after compositor start.
const POST_START_SCRIPT: &str = "post_start.sh";

/// Number of frames considered for best-case rendering times.
const RECENT_FRAME_COUNT: usize = 16;

/// Padding added to render time predictions.
const PREDICTION_PADDING: Duration = Duration::from_millis(8);

/// Shared compositor state.
pub struct Catacomb {
    pub event_loop: LoopHandle<'static, Self>,
    pub display_handle: DisplayHandle,
    pub key_bindings: Vec<KeyBinding>,
    pub touch_state: TouchState,
    pub frame_pacer: FramePacer,
    pub seat_name: String,
    pub windows: Windows,
    pub seat: Seat<Self>,
    pub terminated: bool,
    pub sleeping: bool,
    pub backend: Udev,

    // Smithay state.
    pub idle_notifier_state: IdleNotifierState<Self>,
    pub dmabuf_state: DmabufState,
    keyboard_shortcuts_inhibit_state: KeyboardShortcutsInhibitState,
    primary_selection_state: PrimarySelectionState,
    xdg_activation_state: XdgActivationState,
    kde_decoration_state: KdeDecorationState,
    layer_shell_state: WlrLayerShellState,
    data_control_state: DataControlState,
    lock_state: SessionLockManagerState,
    data_device_state: DataDeviceState,
    compositor_state: CompositorState,
    xdg_shell_state: XdgShellState,
    seat_state: SeatState<Self>,
    shm_state: ShmState,

    accelerometer_token: RegistrationToken,
    idle_inhibitors: Vec<WlSurface>,
    last_focus: Option<WlSurface>,
    locker: Option<SessionLocker>,
    ime_override: Option<bool>,

    // Indicates if rendering was intentionally stalled.
    //
    // This will occur when rendering on a VBlank detects no damage is present, thus stopping
    // rendering and further VBlank. If this is `true`, rendering needs to be kicked of manually
    // again when damage is received.
    stalled: bool,
}

impl Catacomb {
    /// Initialize the compositor.
    pub fn new(event_loop: LoopHandle<'static, Self>, backend: Udev) -> Self {
        let display = Display::new().expect("Wayland display creation");
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
                trace_error(
                    catacomb.display_handle.insert_client(stream, Arc::new(ClientState::default())),
                );
            })
            .expect("register Wayland socket source");

        // Log and set `WAYLAND_DISPLAY` for children.
        env::set_var("WAYLAND_DISPLAY", &socket_name);
        info!("Wayland socket: {socket_name}");

        // Register display event source.
        event_loop
            .insert_source(
                Generic::new(display, Interest::READ, TriggerMode::Level),
                |_, display, catacomb| Ok(catacomb.handle_socket_readiness(display)),
            )
            .expect("register display");

        // Create the compositor and register a surface commit handler.
        let compositor_state = CompositorState::new_v6::<Self>(&display_handle);

        // Setup XDG Shell.
        let xdg_shell_state = XdgShellState::new_with_capabilities::<Self>(&display_handle, [
            WmCapabilities::Fullscreen,
        ]);

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

        SinglePixelBufferState::new::<Self>(&display_handle);

        // Initialize idle-inhibit protocol.
        IdleInhibitManagerState::new::<Self>(&display_handle);

        // Initialize idle-notify protocol.
        let idle_notifier_state = IdleNotifierState::new(&display_handle, event_loop.clone());

        // Initialize session-lock protocol.
        let lock_state = SessionLockManagerState::new::<Self, _>(&display_handle, |_| true);

        // Initalize xdg-activation protocol.
        let xdg_activation_state = XdgActivationState::new::<Self>(&display_handle);

        // Initalize primary-selection protocol.
        let primary_selection_state = PrimarySelectionState::new::<Self>(&display_handle);

        // Initalize keyboard-shortcuts-inhibit protocol.
        let keyboard_shortcuts_inhibit_state =
            KeyboardShortcutsInhibitState::new::<Self>(&display_handle);

        // Initialize seat.
        let seat_name = backend.seat_name();
        let mut seat_state = SeatState::new();
        let mut seat = seat_state.new_wl_seat(&display_handle, seat_name.clone());

        // Initialize IME and virtual keyboard.
        InputMethodManagerState::new::<Self, _>(&display_handle, |_| true);
        TextInputManagerState::new::<Self>(&display_handle);
        VirtualKeyboardManagerState::new::<Self, _>(&display_handle, |_| true);

        // Initialize keyboard/touch/data device.
        let data_device_state = DataDeviceState::new::<Self>(&display_handle);
        seat.add_keyboard(XkbConfig::default(), 200, 25).expect("adding keyboard");

        let data_control_state = DataControlState::new::<Self, _>(
            &display_handle,
            Some(&primary_selection_state),
            |_| true,
        );

        // Initialize touch state.
        seat.add_touch();
        let touch_state = TouchState::new(event_loop.clone());

        // Start IPC socket listener.
        ipc_server::spawn_ipc_socket(&event_loop, &socket_name).expect("spawn IPC socket");

        // Create window manager.
        let windows = Windows::new(&display_handle, event_loop.clone());

        // Subscribe to device orientation changes.
        let accel_token = Accelerometer::new().subscribe(&event_loop, |orientation, catacomb| {
            catacomb.handle_orientation(orientation);
        });

        // Disable accelerometer polling if orientation starts locked.
        if windows.orientation_locked() {
            trace_error(event_loop.disable(&accel_token));
        }

        // Run user startup script.
        if let Some(mut script_path) = dirs::config_dir() {
            script_path.push("catacomb");
            script_path.push(POST_START_SCRIPT);

            if let Err(err) = crate::daemon(script_path.as_os_str(), []) {
                error!("Unable to launch {script_path:?}: {err}");
            }
        }

        Self {
            keyboard_shortcuts_inhibit_state,
            primary_selection_state,
            xdg_activation_state,
            kde_decoration_state,
            idle_notifier_state,
            data_control_state,
            layer_shell_state,
            data_device_state,
            compositor_state,
            xdg_shell_state,
            display_handle,
            dmabuf_state,
            touch_state,
            event_loop,
            lock_state,
            seat_state,
            shm_state,
            seat_name,
            windows,
            backend,
            seat,
            accelerometer_token: accel_token,
            idle_inhibitors: Default::default(),
            key_bindings: Default::default(),
            ime_override: Default::default(),
            frame_pacer: Default::default(),
            last_focus: Default::default(),
            terminated: Default::default(),
            sleeping: Default::default(),
            stalled: Default::default(),
            locker: Default::default(),
        }
    }

    /// Handle Wayland event socket read readiness.
    fn handle_socket_readiness(&mut self, display: &mut NoIoDrop<Display<Catacomb>>) -> PostAction {
        unsafe {
            display.get_mut().dispatch_clients(self).expect("Wayland dispatch error");
        }
        PostAction::Continue
    }

    /// Handle everything necessary to draw a single frame.
    #[cfg_attr(feature = "profiling", profiling::function)]
    pub fn create_frame(&mut self) {
        // Skip rendering while the screen is off.
        if self.sleeping {
            self.stalled = true;
            return;
        }

        let frame_start = Instant::now();

        // Clear rendering stall status.
        self.stalled = false;

        // Ensure no redraws are queued beyond this one.
        self.backend.cancel_scheduled_redraws();

        // Update transaction before rendering to update device orientation.
        let transaction_deadline = self.windows.update_transaction();

        // Update surface focus.
        let focus = self.windows.focus().map(|(surface, _)| surface);
        if focus != self.last_focus {
            self.last_focus.clone_from(&focus);
            self.focus(focus);
        }

        // Update idle inhibition state.
        let mut inhibitors = self.idle_inhibitors.iter();
        let inhibited = inhibitors.any(|surface| self.windows.surface_visible(surface));
        self.idle_notifier_state.set_is_inhibited(inhibited);

        // Redraw only when there is damage present.
        if self.windows.damaged() {
            // Apply pending client updates.
            if let Some(renderer) = self.backend.renderer() {
                self.windows.import_buffers(renderer);
            }

            // Draw all visible clients.
            let rendered = self.backend.render(&mut self.windows);

            // Update render time prediction.
            let frame_interval = self.output().frame_interval();
            self.frame_pacer.add_frame(frame_start.elapsed(), frame_interval);

            // Create artificial VBlank if renderer didn't draw.
            //
            // This is necessary, since rendering might have been skipped due to DRM planes
            // and the next frame could still contain more damage like overview animations.
            if !rendered {
                let frame_interval = self.windows.output().frame_interval();
                self.backend.schedule_redraw(frame_interval);
            } else if let Some(locker) = self.locker.take() {
                // Update session lock after successful draw.
                locker.lock();
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
    fn focus(&mut self, surface: Option<WlSurface>) {
        if let Some(keyboard) = self.seat.get_keyboard() {
            keyboard.set_focus(self, surface, SERIAL_COUNTER.next_serial());
        }
    }

    /// Get the last focused surface.
    pub fn last_focus(&self) -> Option<&WlSurface> {
        self.last_focus.as_ref()
    }

    /// Start rendering again if we're currently stalled.
    pub fn unstall(&mut self) {
        if self.stalled {
            self.create_frame();
        }
    }

    /// Completely redraw the screen, ignoring damage.
    ///
    /// The `ignore_unstalled` flag controls whether an immediate redraw should
    /// be performed even if the rendering hasn't stalled.
    pub fn force_redraw(&mut self, ignore_unstalled: bool) {
        self.backend.reset_buffer_ages();
        self.windows.set_dirty();

        if ignore_unstalled {
            self.create_frame();
        } else {
            self.unstall();
        }
    }

    /// Set output power mode.
    pub fn set_sleep(&mut self, sleep: bool) {
        self.sleeping = sleep;

        // Pause accelerometer checks during sleep.
        if sleep {
            trace_error(self.event_loop.disable(&self.accelerometer_token));
        } else if !self.windows.orientation_locked() {
            trace_error(self.event_loop.enable(&self.accelerometer_token));
        }

        self.backend.set_sleep(sleep);
    }

    /// Lock the output's orientation.
    pub fn lock_orientation(&mut self, orientation: Option<Orientation>) {
        trace_error(self.event_loop.disable(&self.accelerometer_token));
        self.windows.lock_orientation(orientation);
        self.unstall();
    }

    /// Unlock the output's orientation.
    pub fn unlock_orientation(&mut self) {
        trace_error(self.event_loop.enable(&self.accelerometer_token));
        self.windows.unlock_orientation();
        self.unstall();
    }

    /// Toggle IME force enable/disable.
    pub fn toggle_ime_override(&mut self) {
        // Find next state and corresponding action.
        //
        // If one state doesn't have an action, it is skipped. If two states don't have
        // an action, we don't do anything.
        let VkActions { enable, disable, auto } = self.vk_actions();
        let (new_override, (program, args)) = match (self.ime_override, enable, disable, auto) {
            (None, _, Some(disable), _) | (Some(true), _, Some(disable), None) => {
                (Some(false), disable)
            },
            (None, Some(enable), ..) | (Some(false), Some(enable), ..) => (Some(true), enable),
            (Some(false), _, _, Some(auto)) | (Some(true), _, _, Some(auto)) => (None, auto),
            // Ignore toggle if there are less than two states to toggle between.
            _ => return,
        };

        // Execute binding to toggle virtual keyboard override.
        if let Err(err) = daemon::spawn(program, args) {
            error!("Failed to update virtual keyboard state ({program} {args:?}): {err}");
        }

        self.ime_override = new_override;

        // Ensure gesture handle is updated.
        self.windows.set_ime_override(self.ime_override);
    }

    /// Get actions for setting virtual keyboard state to enabled/disabled/auto.
    fn vk_actions(&self) -> VkActions<'_> {
        let mut actions = VkActions::default();

        for binding in &self.key_bindings {
            let action = (&binding.program, &binding.arguments);
            match binding.key {
                Keysym::EnableVirtualKeyboard => actions.enable = Some(action),
                Keysym::DisableVirtualKeyboard => actions.disable = Some(action),
                Keysym::AutoVirtualKeyboard => actions.auto = Some(action),
                _ => (),
            }

            // Stop once all actions were found.
            if actions.enable.is_some() && actions.disable.is_some() && actions.auto.is_some() {
                break;
            }
        }

        actions
    }
}

impl CompositorHandler for Catacomb {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        match client.get_data::<ClientState>() {
            Some(state) => &state.compositor_state,
            None => panic!("unknown client data type"),
        }
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
        notifier: ImportNotifier,
    ) {
        let renderer = match self.backend.renderer() {
            Some(renderer) => renderer,
            None => return,
        };

        match renderer.import_dmabuf(&buffer, None) {
            Err(err) => {
                notifier.failed();
                error!("Buffer import failed: {err}");
            },
            Ok(_) => {
                let _ = notifier.successful::<Self>();
            },
        }
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

    fn new_popup(&mut self, surface: PopupSurface, _positioner: PositionerState) {
        self.windows.add_popup(surface);
    }

    fn reposition_request(
        &mut self,
        surface: PopupSurface,
        positioner: PositionerState,
        token: u32,
    ) {
        // Update positioner.
        surface.with_pending_state(|state| state.positioner = positioner);

        // Handle popup constraints and send configure.
        self.windows.reposition_popup(&surface, token);
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
        namespace: String,
    ) {
        self.windows.add_layer(layer, surface, namespace);
    }

    fn layer_destroyed(&mut self, surface: LayerSurface) {
        self.windows.reap_layer(&surface);
        self.unstall();
    }
}
delegate_layer_shell!(Catacomb);

impl SessionLockHandler for Catacomb {
    fn lock_state(&mut self) -> &mut SessionLockManagerState {
        &mut self.lock_state
    }

    fn lock(&mut self, confirmation: SessionLocker) {
        self.locker = Some(confirmation);
        self.windows.lock();
        self.unstall();
    }

    fn unlock(&mut self) {
        self.windows.unlock();
    }

    fn new_surface(&mut self, surface: LockSurface, _output: WlOutput) {
        self.windows.set_lock_surface(surface);
    }
}
delegate_session_lock!(Catacomb);

impl SeatHandler for Catacomb {
    type KeyboardFocus = WlSurface;
    type PointerFocus = WlSurface;
    type TouchFocus = WlSurface;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.seat_state
    }

    fn focus_changed(&mut self, seat: &Seat<Self>, surface: Option<&Self::KeyboardFocus>) {
        // Update data device focus.
        let client = surface.and_then(|surface| self.display_handle.get_client(surface.id()).ok());
        data_device::set_data_device_focus(&self.display_handle, seat, client.clone());

        // Update primary selection focus.
        primary_selection::set_primary_focus(&self.display_handle, &self.seat, client);
    }
}
delegate_seat!(Catacomb);

impl InputMethodHandler for Catacomb {
    fn new_popup(&mut self, _surface: ImeSurface) {}

    fn dismiss_popup(&mut self, _surface: ImeSurface) {}

    fn popup_repositioned(&mut self, _surface: ImeSurface) {}

    fn parent_geometry(&self, parent: &WlSurface) -> Rectangle<i32, Logical> {
        self.windows.parent_geometry(parent)
    }
}

delegate_virtual_keyboard_manager!(Catacomb);
delegate_input_method_manager!(Catacomb);
delegate_text_input_manager!(Catacomb);

impl OutputHandler for Catacomb {}

delegate_output!(Catacomb);

impl XdgDecorationHandler for Catacomb {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        toplevel.set_state(|state| {
            state.decoration_mode = Some(DecorationMode::ServerSide);
        });
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
        self.windows.set_dirty();
        self.unstall();
    }
}
delegate_screencopy!(Catacomb);

impl IdleInhibitHandler for Catacomb {
    fn inhibit(&mut self, surface: WlSurface) {
        self.idle_inhibitors.push(surface.clone());
    }

    fn uninhibit(&mut self, surface: WlSurface) {
        self.idle_inhibitors.retain(|inhibitor_surface| {
            inhibitor_surface.is_alive() && inhibitor_surface != &surface
        });
    }
}
delegate_idle_inhibit!(Catacomb);

impl IdleNotifierHandler for Catacomb {
    fn idle_notifier_state(&mut self) -> &mut IdleNotifierState<Self> {
        &mut self.idle_notifier_state
    }
}
delegate_idle_notify!(Catacomb);

impl FractionalScaleHandler for Catacomb {
    fn new_fractional_scale(&mut self, surface: WlSurface) {
        // Submit last cached preferred scale.
        compositor::with_states(&surface, |states| {
            fractional_scale::with_fractional_scale(states, |fractional_scale| {
                if let Some(surface_data) = states.data_map.get::<RefCell<CatacombSurfaceData>>() {
                    let scale = surface_data.borrow().preferred_fractional_scale;
                    fractional_scale.set_preferred_scale(scale);
                }
            });
        });
    }
}
delegate_fractional_scale!(Catacomb);

impl XdgActivationHandler for Catacomb {
    fn activation_state(&mut self) -> &mut XdgActivationState {
        &mut self.xdg_activation_state
    }

    fn token_created(&mut self, _token: XdgActivationToken, data: XdgActivationTokenData) -> bool {
        let (serial, seat, keyboard) = match (data.serial, self.seat.get_keyboard()) {
            (Some((serial, seat)), Some(keyboard)) => (serial, seat, keyboard),
            _ => return false,
        };

        Seat::from_resource(&seat).as_ref() == Some(&self.seat)
            && keyboard
                .last_enter()
                .map_or(false, |last_enter| serial.is_no_older_than(&last_enter))
    }

    fn request_activation(
        &mut self,
        _token: XdgActivationToken,
        token_data: XdgActivationTokenData,
        surface: WlSurface,
    ) {
        // Ignore tokens which are too old.
        if token_data.timestamp.elapsed() >= ACTIVATION_TIMEOUT {
            return;
        }

        // Select raise/urgency based on focus of the client which created the token.
        if token_data.surface == self.last_focus {
            self.windows.raise(&surface);
            self.windows.set_dirty();
            self.unstall();
        } else if Some(&surface) != self.last_focus.as_ref() {
            self.windows.set_urgent(&surface, true);
            self.windows.set_dirty();
            self.unstall();
        }
    }
}
delegate_xdg_activation!(Catacomb);

impl SelectionHandler for Catacomb {
    type SelectionUserData = ();
}

impl PrimarySelectionHandler for Catacomb {
    fn primary_selection_state(&self) -> &PrimarySelectionState {
        &self.primary_selection_state
    }
}
delegate_primary_selection!(Catacomb);

impl DataDeviceHandler for Catacomb {
    fn data_device_state(&self) -> &DataDeviceState {
        &self.data_device_state
    }
}
impl ClientDndGrabHandler for Catacomb {}
impl ServerDndGrabHandler for Catacomb {}
delegate_data_device!(Catacomb);

impl DataControlHandler for Catacomb {
    fn data_control_state(&self) -> &DataControlState {
        &self.data_control_state
    }
}
delegate_data_control!(Catacomb);

impl KeyboardShortcutsInhibitHandler for Catacomb {
    fn keyboard_shortcuts_inhibit_state(&mut self) -> &mut KeyboardShortcutsInhibitState {
        &mut self.keyboard_shortcuts_inhibit_state
    }

    fn new_inhibitor(&mut self, inhibitor: KeyboardShortcutsInhibitor) {
        // Mark surface as inhibiting.
        compositor::with_states(inhibitor.wl_surface(), |states| {
            let surface_data =
                states.data_map.get_or_insert(|| RefCell::new(CatacombSurfaceData::new()));
            surface_data.borrow_mut().inhibits_shortcuts = true;
        });

        inhibitor.activate();
    }

    fn inhibitor_destroyed(&mut self, inhibitor: KeyboardShortcutsInhibitor) {
        // Remove inhibition flag from the surface.
        compositor::with_states(inhibitor.wl_surface(), |states| {
            let surface_data =
                states.data_map.get_or_insert(|| RefCell::new(CatacombSurfaceData::new()));
            surface_data.borrow_mut().inhibits_shortcuts = false;
        });
    }
}
delegate_keyboard_shortcuts_inhibit!(Catacomb);

delegate_presentation!(Catacomb);

delegate_viewporter!(Catacomb);

/// Unused state to make Smithay happy.
#[derive(Default)]
struct ClientState {
    compositor_state: CompositorClientState,
}

impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}

    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {}
}

delegate_single_pixel_buffer!(Catacomb);

/// Compositor rendering time prediction.
#[derive(Default)]
pub struct FramePacer {
    recent_frames: [u64; RECENT_FRAME_COUNT],
    frame_count: u64,
    total_ns: u64,
}

impl FramePacer {
    /// Add a new frame to the tracker.
    fn add_frame(&mut self, render_time: Duration, frame_interval: Duration) {
        // Limit worst-case to refresh rate, to reduce impact of statistical outliers.
        let ns = cmp::min(render_time, frame_interval).as_nanos() as u64;

        self.recent_frames.rotate_right(1);
        self.recent_frames[0] = ns;

        self.total_ns += ns;

        self.frame_count += 1;
    }

    /// Predict time required for next frame.
    pub fn predict(&self) -> Option<Duration> {
        if self.frame_count == 0 {
            return None;
        }

        // Use total average as the minimum render time.
        let min_time_ns = self.total_ns / self.frame_count;

        // Use longest recent frame as baseline.
        let recent_frames = self.recent_frames.iter().copied().take(self.frame_count as usize);
        let recent_worst_ns = recent_frames.max().unwrap_or_default();

        let prediction = Duration::from_nanos(recent_worst_ns.max(min_time_ns));

        // Add buffer to account for measuring tolerances.
        Some(prediction + PREDICTION_PADDING)
    }
}

/// Programs for controlling virtual keyboard state.
#[derive(Default)]
struct VkActions<'a> {
    enable: Option<(&'a String, &'a Vec<String>)>,
    disable: Option<(&'a String, &'a Vec<String>)>,
    auto: Option<(&'a String, &'a Vec<String>)>,
}
