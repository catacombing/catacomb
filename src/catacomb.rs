//! Catacomb compositor state.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;
use std::{env, io, mem};

use _decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;
use _server_decoration::server::org_kde_kwin_server_decoration_manager::Mode as ManagerMode;
use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::renderer::ImportDma;
use smithay::reexports::calloop::generic::Generic;
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
use smithay::utils::{Physical, Rectangle};
use smithay::wayland::buffer::BufferHandler;
use smithay::wayland::compositor::{CompositorHandler, CompositorState};
use smithay::wayland::data_device::{
    ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
};
use smithay::wayland::dmabuf::{DmabufGlobal, DmabufHandler, DmabufState, ImportError};
use smithay::wayland::input_method::{InputMethodManagerState, InputMethodSeat};
use smithay::wayland::output::OutputManagerState;
use smithay::wayland::seat::{KeyboardHandle, Seat, SeatHandler, SeatState, XkbConfig};
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
use smithay::wayland::virtual_keyboard::VirtualKeyboardManagerState;
use smithay::wayland::{compositor, data_device, Serial, SERIAL_COUNTER};
use smithay::{
    delegate_compositor, delegate_data_device, delegate_dmabuf, delegate_input_method_manager,
    delegate_kde_decoration, delegate_layer_shell, delegate_output, delegate_seat, delegate_shm,
    delegate_text_input_manager, delegate_virtual_keyboard_manager, delegate_xdg_decoration,
    delegate_xdg_shell,
};

use crate::drawing::MAX_DAMAGE_AGE;
use crate::input::{PhysicalButtonState, TouchState};
use crate::orientation::{Accelerometer, AccelerometerSource};
use crate::windows::Windows;
use crate::{daemon, Udev};

/// The script to run after compositor start.
const POST_START_SCRIPT: &str = "post_start.sh";

/// Shared compositor state.
pub struct Catacomb {
    pub suspend_timer: Option<RegistrationToken>,
    pub event_loop: LoopHandle<'static, Self>,
    pub button_state: PhysicalButtonState,
    pub display_handle: DisplayHandle,
    pub keyboard: KeyboardHandle,
    pub touch_state: TouchState,
    pub seat_name: String,
    pub windows: Windows,
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

    last_focus: Option<WlSurface>,
    seat: Seat<Self>,
    damage: Damage,

    // NOTE: Must be last field to ensure it's dropped after any global.
    pub display: Rc<RefCell<Display<Self>>>,
}

impl Catacomb {
    /// Initialize the compositor.
    pub fn new(event_loop: LoopHandle<'static, Self>, backend: Udev) -> Self {
        let mut display = Display::new().expect("Wayland display creation");
        let display_handle = display.handle();

        // Create and register Wayland socket.
        let socket_source = ListeningSocketSource::new_auto(None).expect("create Wayland socket");
        let socket_name = socket_source.socket_name().to_string_lossy().into_owned();
        event_loop
            .insert_source(socket_source, move |stream, _, catacomb| {
                let _ = catacomb.display_handle.insert_client(stream, Arc::new(()));
            })
            .expect("register Wayland socket source");

        // Log and set `WAYLAND_DISPLAY` for children.
        env::set_var("WAYLAND_DISPLAY", &socket_name);
        println!("Wayland socket: {}", socket_name);

        // Register display event source.
        event_loop
            .insert_source(
                Generic::new(display.backend().poll_fd(), Interest::READ, TriggerMode::Level),
                |_, _, catacomb| catacomb.handle_socket_readiness(),
            )
            .expect("register display");

        // Create the compositor and register a surface commit handler.
        let compositor_state = CompositorState::new::<Self, _>(&display_handle, None);

        // Setup XDG Shell.
        let xdg_shell_state = XdgShellState::new::<Self, _>(&display_handle, None);

        // Setup layer shell.
        let layer_shell_state = WlrLayerShellState::new::<Self, _>(&display_handle, None);

        // Advertise support for rendering from CPU-based shared memory buffers.
        let shm_state = ShmState::new::<Self, _>(&display_handle, Vec::new(), None);

        // Advertise support for rendering GPU-based buffers.
        let dmabuf_state = DmabufState::new();

        // XDG output protocol.
        OutputManagerState::new_with_xdg_output::<Self>(&display_handle);

        // Force server-side decorations.
        XdgDecorationState::new::<Self, _>(&display_handle, None);
        let kde_decoration_state =
            KdeDecorationState::new::<Self, _>(&display_handle, ManagerMode::Server, None);

        // Initialize seat.
        let seat_name = backend.seat_name();
        let seat_state = SeatState::new();
        let mut seat = Seat::new(&display_handle, seat_name.clone(), None);

        // Initialize IME and virtual keyboard.
        InputMethodManagerState::new::<Self>(&display_handle);
        seat.add_input_method(XkbConfig::default(), 200, 25);
        TextInputManagerState::new::<Self>(&display_handle);
        VirtualKeyboardManagerState::new::<Self>(&display_handle);

        // Initialize keyboard/touch/data device.
        let data_device_state = DataDeviceState::new::<Self, _>(&display_handle, None);
        let keyboard_display = display_handle.clone();
        let keyboard = seat
            .add_keyboard(XkbConfig::default(), 200, 25, move |seat, focused_surface| {
                let client = focused_surface
                    .and_then(|surface| keyboard_display.get_client(surface.id()).ok());
                data_device::set_data_device_focus(&keyboard_display, seat, client)
            })
            .expect("adding keyboard");

        // Initialize touch state.
        let touch = seat.add_touch();
        let touch_state = TouchState::new(event_loop.clone(), touch);

        // Subscribe to device orientation changes.
        Accelerometer::new().subscribe(&event_loop, |orientation, catacomb| {
            catacomb.handle_orientation(orientation);
        });

        // Run user startup script.
        if let Some(mut script_path) = dirs::config_dir() {
            script_path.push("catacomb");
            script_path.push(POST_START_SCRIPT);
            let _ = daemon::spawn(script_path.as_os_str(), []);
        }

        // Create window manager.
        let windows = Windows::new(&display_handle);

        Self {
            kde_decoration_state,
            layer_shell_state,
            data_device_state,
            compositor_state,
            xdg_shell_state,
            display_handle,
            dmabuf_state,
            touch_state,
            event_loop,
            seat_state,
            shm_state,
            seat_name,
            keyboard,
            windows,
            backend,
            seat,
            display: Rc::new(RefCell::new(display)),
            suspend_timer: Default::default(),
            button_state: Default::default(),
            last_focus: Default::default(),
            sleeping: Default::default(),
            damage: Default::default(),
        }
    }

    /// Handle Wayland event socket read readiness.
    fn handle_socket_readiness(&mut self) -> io::Result<PostAction> {
        let display = self.display.clone();
        let mut display = display.borrow_mut();
        display.dispatch_clients(self).expect("Wayland dispatch error");
        Ok(PostAction::Continue)
    }
}

impl Catacomb {
    /// Handle everything necessary to draw a single frame.
    pub fn create_frame(&mut self) {
        // Update transaction before rendering to update device orientation.
        self.windows.update_transaction();

        // Update surface focus.
        let focus = self.windows.focus();
        if focus != self.last_focus {
            self.last_focus = focus.clone();
            self.focus(focus.as_ref());
        }

        // Redraw only when there is damage present.
        if self.windows.damaged() {
            self.backend.render(&mut self.windows, &mut self.damage);
        } else {
            self.backend.schedule_redraw(self.windows.output.frame_interval());
        }

        // Handle window liveliness changes.
        self.windows.refresh();

        // Request new frames for visible windows.
        self.windows.request_frames();
    }

    /// Focus a new surface.
    pub fn focus(&mut self, surface: Option<&WlSurface>) {
        if let Some(text_input) = self.seat.user_data().get::<TextInputHandle>() {
            text_input.set_focus(surface);
        }
        self.keyboard.set_focus(&self.display_handle, surface, SERIAL_COUNTER.next_serial());
    }
}

impl CompositorHandler for Catacomb {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn commit(&mut self, _display: &DisplayHandle, surface: &WlSurface) {
        if compositor::is_sync_subsurface(surface) {
            return;
        }

        self.windows.surface_commit(surface);
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
        _display: &DisplayHandle,
        _global: &DmabufGlobal,
        buffer: Dmabuf,
    ) -> Result<(), ImportError> {
        let output_device = match &mut self.backend.output_device {
            Some(output_device) => output_device,
            None => return Err(ImportError::Failed),
        };

        output_device.renderer.import_dmabuf(&buffer, None).map_err(|_| ImportError::Failed)?;
        Ok(())
    }
}
delegate_dmabuf!(Catacomb);

impl XdgShellHandler for Catacomb {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, _display: &DisplayHandle, surface: ToplevelSurface) {
        self.windows.add(surface);
    }

    fn ack_configure(
        &mut self,
        _display: &DisplayHandle,
        surface: WlSurface,
        _configure: Configure,
    ) {
        // Request new frames after each resize.
        let runtime = self.windows.runtime();
        if let Some(mut window) = self.windows.find_xdg(&surface) {
            window.request_frame(runtime);
        }
    }

    fn new_popup(
        &mut self,
        _display: &DisplayHandle,
        surface: PopupSurface,
        _positioner: PositionerState,
    ) {
        self.windows.add_popup(surface);
    }

    fn grab(
        &mut self,
        _display: &DisplayHandle,
        _surface: PopupSurface,
        _seat: WlSeat,
        _serial: Serial,
    ) {
    }
}
delegate_xdg_shell!(Catacomb);

impl WlrLayerShellHandler for Catacomb {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.layer_shell_state
    }

    fn new_layer_surface(
        &mut self,
        _display: &DisplayHandle,
        surface: LayerSurface,
        _wl_output: Option<WlOutput>,
        layer: Layer,
        _namespace: String,
    ) {
        self.windows.add_layer(layer, surface);
    }
}
delegate_layer_shell!(Catacomb);

impl SeatHandler for Catacomb {
    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.seat_state
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
    fn new_decoration(&mut self, _display: &DisplayHandle, toplevel: ToplevelSurface) {
        toplevel.with_pending_state(|state| {
            state.decoration_mode = Some(DecorationMode::ServerSide);
        });
        toplevel.send_configure();
    }

    fn request_mode(
        &mut self,
        _display: &DisplayHandle,
        _toplevel: ToplevelSurface,
        _mode: DecorationMode,
    ) {
    }

    fn unset_mode(&mut self, _display: &DisplayHandle, _toplevel: ToplevelSurface) {}
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

#[derive(Default, Debug)]
pub struct Damage {
    /// Combined damage history for all tracked buffer ages.
    damage: Vec<Rectangle<i32, Physical>>,

    /// Tracked damage rectangles per buffer age.
    ///
    /// This must be one bigger than [`MAX_DAMAGE_AGE`], since the last slot is
    /// reserved for pending damage for the next frame.
    rects: [usize; MAX_DAMAGE_AGE + 1],

    /// Buffer for storing current damage.
    ///
    /// This is used to deduplicate the damage rectangles to prevent excessive
    /// draw calls.
    current: Vec<Rectangle<i32, Physical>>,
}

impl Damage {
    /// Add pending damage for the next frame.
    pub fn push(&mut self, damage: Rectangle<i32, Physical>) {
        self.rects[self.rects.len() - 1] += 1;
        self.damage.push(damage);
    }

    /// Calculate damage history since buffer age.
    ///
    /// This will also clear the pending damage, pushing it into history and
    /// preparing the storage for new damage.
    pub fn take_since(&mut self, buffer_age: u8) -> &[Rectangle<i32, Physical>] {
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
