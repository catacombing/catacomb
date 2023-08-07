//! Input event handling.

use std::time::{Duration, Instant};

use catacomb_ipc::{GestureSector, Modifiers};
use smithay::backend::input::{
    AbsolutePositionEvent, ButtonState, Event, InputBackend, InputEvent, KeyState,
    KeyboardKeyEvent, MouseButton, PointerButtonEvent, TouchEvent as _, TouchSlot,
};
use smithay::input::keyboard::{keysyms, FilterResult, Keysym, KeysymHandle, ModifiersState};
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use smithay::utils::{Logical, Point, Rectangle, SERIAL_COUNTER};
use smithay::wayland::seat::TouchHandle;
use tracing::error;

use crate::catacomb::Catacomb;
use crate::config::GestureBinding;
use crate::daemon;
use crate::orientation::Orientation;
use crate::output::{Canvas, GESTURE_HANDLE_HEIGHT};
use crate::windows::surface::{InputSurface, InputSurfaceKind};

/// Time before a tap is considered a hold.
pub const HOLD_DURATION: Duration = Duration::from_secs(1);

/// Duration after resume for which power button presses will be ignored.
const RESUME_INHIBIT_DURATION: Duration = Duration::from_millis(500);

/// Time before button press is considered a hold.
const BUTTON_HOLD_DURATION: Duration = Duration::from_millis(500);

/// Square of the maximum distance before touch input is considered a drag.
const MAX_TAP_DISTANCE: f64 = 400.;

/// Friction for velocity computation.
const FRICTION: f64 = 0.1;

/// Touch slot for pointer emulation.
///
/// The touch slot `None`, which is usually used for devices that do not support
/// multitouch, does not work properly with some clients like GTK. To work
/// around this we pick an arbitrary multitouch slot instead.
const POINTER_TOUCH_SLOT: Option<u32> = Some(0);

/// Touch input state.
pub struct TouchState {
    pub user_gestures: Vec<GestureBinding>,
    pub position: Point<f64, Logical>,

    event_loop: LoopHandle<'static, Catacomb>,
    velocity_timer: Option<RegistrationToken>,
    input_surface: Option<InputSurface>,
    active_app_id: Option<String>,
    velocity: Point<f64, Logical>,
    events: Vec<TouchEvent>,
    slot: Option<TouchSlot>,
    touch: TouchHandle,
    start: TouchStart,
    is_drag: bool,
}

impl TouchState {
    pub fn new(event_loop: LoopHandle<'static, Catacomb>, touch: TouchHandle) -> Self {
        Self {
            event_loop,
            touch,
            velocity_timer: Default::default(),
            input_surface: Default::default(),
            user_gestures: Default::default(),
            active_app_id: Default::default(),
            position: Default::default(),
            velocity: Default::default(),
            is_drag: Default::default(),
            events: Default::default(),
            start: Default::default(),
            slot: Default::default(),
        }
    }

    /// Stop all touch velocity.
    pub fn cancel_velocity(&mut self) {
        if let Some(velocity_timer) = self.velocity_timer.take() {
            self.event_loop.remove(velocity_timer);
        }
        self.velocity = Default::default();
    }

    /// Start a new touch session.
    fn start(&mut self, canvas: &Canvas, slot: TouchSlot, position: Point<f64, Logical>) {
        // Allow only a single touch at a time.
        if self.slot.is_some() {
            return;
        }
        self.slot = Some(slot);

        self.cancel_velocity();

        self.start = TouchStart::new(canvas, position);
        self.position = position;
        self.is_drag = false;
    }

    /// Check if there's any touch velocity present.
    fn has_velocity(&self) -> bool {
        self.velocity.x.abs() >= f64::EPSILON || self.velocity.y.abs() >= f64::EPSILON
    }

    /// Check if there's currently any touch interaction active.
    #[inline]
    pub fn touching(&self) -> bool {
        self.slot.is_some()
    }

    /// Get the updated active touch action.
    fn action(&mut self, canvas: &Canvas) -> Option<TouchAction> {
        // Process handle gestures even before completion.
        if self.start.is_handle_gesture {
            // Continue gesture if direction is locked in already.
            match self.start.handle_direction {
                Some(HandleDirection::Horizontal) => {
                    let delta = self.position.x - self.start.position.x;
                    return Some(HandleGesture::Horizontal(delta).into());
                },
                Some(HandleDirection::Vertical) => {
                    return Some(HandleGesture::Vertical(self.position.y).into());
                },
                None => (),
            }

            // Lock in direction once threshold was passed.
            let gesture = HandleGesture::from_points(canvas, self.start.position, self.position);
            if let Some(gesture) = gesture {
                self.start.handle_direction = Some(HandleDirection::from(&gesture));
                return Some(gesture.into());
            }
        }

        // Check if a user gesture was completed.
        let touching = self.touching();
        if !touching {
            // Find matching user gestures.
            let app_id = self.active_app_id.as_ref();
            let start = self.start.position;
            let end = Some(self.position);
            let mut gestures = self.matching_gestures(canvas, app_id, start, end);

            if let Some(gesture) = gestures.next() {
                return Some(TouchAction::UserGesture((
                    gesture.program.clone(),
                    gesture.arguments.clone(),
                )));
            }
        }

        // Convert to drag as soon as distance/time was exceeded once.
        let delta = self.start.position - self.position;
        if self.is_drag
            || delta.x.powi(2) + delta.y.powi(2) > MAX_TAP_DISTANCE
            || self.start.time.elapsed() >= HOLD_DURATION
        {
            self.is_drag = true;
            return Some(TouchAction::Drag);
        }

        (!touching).then_some(TouchAction::Tap)
    }

    /// Find gestures matching an origin point.
    fn matching_gestures<'a>(
        &'a self,
        canvas: &'a Canvas,
        app_id: Option<&'a String>,
        start: Point<f64, Logical>,
        end: Option<Point<f64, Logical>>,
    ) -> impl Iterator<Item = &'a GestureBinding> {
        let canvas_size = canvas.size().to_f64();
        let start_sector = GestureSector::from_point(canvas_size, start);
        let end_sector = end.map(|end| GestureSector::from_point(canvas_size, end));

        self.user_gestures.iter().filter(move |gesture| {
            gesture.start == start_sector
                && end_sector.map_or(true, |sector| gesture.end == sector)
                && gesture.app_id.matches(app_id)
        })
    }
}

/// Start of a touch interaction.
#[derive(Debug)]
struct TouchStart {
    handle_direction: Option<HandleDirection>,
    position: Point<f64, Logical>,
    is_handle_gesture: bool,
    time: Instant,
}

impl Default for TouchStart {
    fn default() -> Self {
        Self {
            time: Instant::now(),
            is_handle_gesture: Default::default(),
            handle_direction: Default::default(),
            position: Default::default(),
        }
    }
}

impl TouchStart {
    fn new(canvas: &Canvas, position: Point<f64, Logical>) -> Self {
        Self {
            position,
            is_handle_gesture: HandleGesture::is_start(canvas, position),
            time: Instant::now(),
            handle_direction: Default::default(),
        }
    }
}

/// Available touch input actions.
#[derive(Debug)]
enum TouchAction {
    HandleGesture(HandleGesture),
    UserGesture((String, Vec<String>)),
    Drag,
    Tap,
}

impl From<HandleGesture> for TouchAction {
    fn from(gesture: HandleGesture) -> Self {
        Self::HandleGesture(gesture)
    }
}

/// Gesture handle touch gestures.
#[derive(Debug, Copy, Clone)]
pub enum HandleGesture {
    /// Vertical position of the current touch point.
    Vertical(f64),
    /// Horizontal distance traveled as delta.
    Horizontal(f64),
}

impl HandleGesture {
    /// Get a handle gesture from its start/end.
    ///
    /// This function assumes that the start is within the gesture handle.
    fn from_points(
        canvas: &Canvas,
        start: Point<f64, Logical>,
        end: Point<f64, Logical>,
    ) -> Option<Self> {
        // Ignore handle gestures until minimum length is exceeded.
        let delta = end - start;
        let squared_length = delta.x.powi(2) + delta.y.powi(2);
        if squared_length < MAX_TAP_DISTANCE {
            return None;
        }

        if delta.y.abs() >= delta.x.abs() {
            Some(HandleGesture::Vertical(end.y))
        } else if Self::is_start(canvas, end) {
            Some(HandleGesture::Horizontal(delta.x))
        } else {
            None
        }
    }

    /// Check if a touch should start a new gesture.
    fn is_start(canvas: &Canvas, position: Point<f64, Logical>) -> bool {
        let canvas_size = canvas.size().to_f64();
        let loc = (0., canvas_size.h - GESTURE_HANDLE_HEIGHT as f64);
        Rectangle::from_loc_and_size(loc, canvas_size).contains(position)
    }
}

/// Direction for a handle gesture.
#[derive(Debug)]
enum HandleDirection {
    Horizontal,
    Vertical,
}

impl From<&HandleGesture> for HandleDirection {
    fn from(gesture: &HandleGesture) -> Self {
        match gesture {
            HandleGesture::Horizontal(_) => Self::Horizontal,
            HandleGesture::Vertical(_) => Self::Vertical,
        }
    }
}

/// Generic touch event.
#[derive(Copy, Clone, Debug)]
struct TouchEvent {
    position: Point<f64, Logical>,
    ty: TouchEventType,
    slot: TouchSlot,
    time: u32,
}

impl TouchEvent {
    fn new(ty: TouchEventType, slot: TouchSlot, time: u32, position: Point<f64, Logical>) -> Self {
        Self { slot, time, position, ty }
    }
}

/// Types of touch event.
#[derive(Copy, Clone, Debug)]
enum TouchEventType {
    Down,
    Up,
    Motion,
}

/// Hardware button state.
#[derive(Default)]
pub struct PhysicalButtonState {
    power: Option<usize>,
    next_id: usize,
}

impl PhysicalButtonState {
    fn next_id(&mut self) -> usize {
        self.next_id += 1;
        self.next_id
    }
}

impl Catacomb {
    /// Process device orientation changes.
    pub fn handle_orientation(&mut self, orientation: Orientation) {
        self.windows.update_orientation(orientation);
        self.force_redraw(false);
    }

    /// Process new input events.
    pub fn handle_input<I: InputBackend>(&mut self, event: InputEvent<I>) {
        // Ignore non-keyboard input events while the screen is off.
        if self.sleeping && !matches!(event, InputEvent::Keyboard { .. }) {
            return;
        }

        // Reset idle sleep timer.
        self.reset_idle_timer();

        match event {
            InputEvent::Keyboard { event, .. } => self.on_keyboard_input(&event),
            InputEvent::PointerButton { event } if event.button() == Some(MouseButton::Left) => {
                let slot = TouchSlot::from(POINTER_TOUCH_SLOT);
                let position = self.touch_state.position;
                if event.state() == ButtonState::Pressed {
                    self.on_touch_down(TouchEvent::new(TouchEventType::Down, slot, 0, position));
                } else {
                    self.on_touch_up(TouchEvent::new(TouchEventType::Up, slot, 0, position));
                }
            },
            InputEvent::PointerMotionAbsolute { event } => {
                let position = self.transform_position(&event);
                self.touch_state.position = position;

                if self.touch_state.slot.is_some() {
                    let slot = TouchSlot::from(POINTER_TOUCH_SLOT);
                    self.on_touch_motion(TouchEvent::new(TouchEventType::Down, slot, 0, position));
                }
            },
            InputEvent::TouchDown { event } => {
                let position = self.transform_position(&event);
                let event_type = TouchEventType::Down;
                let event = TouchEvent::new(event_type, event.slot(), event.time_msec(), position);
                self.touch_state.events.push(event);
            },
            InputEvent::TouchUp { event } => {
                let position = self.touch_state.position;
                let event_type = TouchEventType::Up;
                let event = TouchEvent::new(event_type, event.slot(), event.time_msec(), position);
                self.touch_state.events.push(event);
            },
            InputEvent::TouchMotion { event } => {
                let position = self.transform_position(&event);
                let event_type = TouchEventType::Motion;
                let event = TouchEvent::new(event_type, event.slot(), event.time_msec(), position);
                self.touch_state.events.push(event);
            },
            // Apply all pending touch events.
            InputEvent::TouchFrame { .. } => {
                for i in 0..self.touch_state.events.len() {
                    let event = self.touch_state.events[i];
                    match event.ty {
                        TouchEventType::Down => self.on_touch_down(event),
                        TouchEventType::Up => self.on_touch_up(event),
                        TouchEventType::Motion => self.on_touch_motion(event),
                    }
                }
                self.touch_state.events.clear();
            },
            // Handle gesture touch cancel for nested compositors.
            InputEvent::TouchCancel { event } => {
                self.touch_state.events.retain(|touch_event| touch_event.slot != event.slot());
            },
            _ => (),
        }

        // Wakeup rendering.
        self.unstall();
    }

    /// Handle new touch input start.
    fn on_touch_down(&mut self, event: TouchEvent) {
        let TouchEvent { time, slot, position, .. } = event;

        // Initialize the touch state.
        self.touch_state.start(self.windows.canvas(), slot, position);

        // Find surface at touch position.
        let surface = self.windows.surface_at(event.position);

        // Notify client.
        self.touch_state.input_surface = None;
        self.touch_state.active_app_id = None;
        if let Some(mut input_surface) = surface {
            // Get surface's App ID.
            let app_id = input_surface.toplevel.as_mut().and_then(InputSurfaceKind::take_app_id);

            // Check if a gesture is triggered by this touch event.
            let gesture_active = self.touch_state.start.is_handle_gesture
                || self
                    .touch_state
                    .matching_gestures(self.windows.canvas(), app_id.as_ref(), event.position, None)
                    .next()
                    .is_some();
            self.touch_state.active_app_id = app_id;

            if !gesture_active {
                // Update window focus.
                match input_surface.toplevel.take() {
                    Some(InputSurfaceKind::Layout((window, _))) => {
                        self.windows.set_focus(Some(window), None, None);
                    },
                    Some(InputSurfaceKind::Layer((layer, app_id))) => {
                        self.windows.set_focus(None, Some(layer), app_id);
                    },
                    // For surfaces denying focus, we send events but inhibit focus.
                    None => (),
                }

                // Calculate surface-local touch position.
                let scale = self.windows.canvas().scale();
                let position = input_surface.local_position(scale, event.position);

                // Send touch event to the client.
                let serial = SERIAL_COUNTER.next_serial();
                let surface = &input_surface.surface;
                self.touch_state.touch.down(serial, time, surface, position, slot);

                self.touch_state.input_surface = Some(input_surface);
            }
        }

        // Only send touch start if there's no handle gesture in progress.
        if !self.touch_state.start.is_handle_gesture {
            self.windows.on_touch_start(position);
        }
    }

    /// Handle touch input release.
    fn on_touch_up(&mut self, event: TouchEvent) {
        // Notify client.
        let serial = SERIAL_COUNTER.next_serial();
        self.touch_state.touch.up(serial, event.time, event.slot);

        // Check if slot is the active one.
        if self.touch_state.slot != Some(event.slot) {
            return;
        }
        self.touch_state.slot = None;

        match self.touch_state.action(self.windows.canvas()) {
            Some(TouchAction::Tap) => {
                // Clear focus when tapping outside of any window.
                if self.touch_state.input_surface.is_none() {
                    self.windows.set_focus(None, None, None);
                }

                self.windows.on_tap(self.touch_state.position);
            },
            Some(
                TouchAction::Drag | TouchAction::HandleGesture(_) | TouchAction::UserGesture(_),
            ) => {
                self.add_velocity_timeout();
                self.update_position(self.touch_state.position);
            },
            None => self.add_velocity_timeout(),
        }
    }

    /// Handle touch input movement.
    fn on_touch_motion(&mut self, event: TouchEvent) {
        // Handle client input.
        if let Some(input_surface) = &self.touch_state.input_surface {
            let scale = self.windows.canvas().scale();
            let position = input_surface.local_position(scale, event.position);
            self.touch_state.touch.motion(event.time, event.slot, position);
            return;
        }

        // Ignore anything but the active touch slot.
        if self.touch_state.slot != Some(event.slot) {
            return;
        }

        self.touch_state.velocity = event.position - self.touch_state.position;
        self.update_position(event.position);
    }

    /// Update the touch position.
    ///
    /// NOTE: This should be called after adding new timeouts to allow clearing
    /// them instead of creating a loop which continuously triggers these
    /// actions.
    fn update_position(&mut self, position: Point<f64, Logical>) {
        match self.touch_state.action(self.windows.canvas()) {
            Some(TouchAction::Drag) => {
                self.windows.on_drag(&mut self.touch_state, position);

                // Signal drag end once no more velocity is present.
                if !self.touch_state.touching() && !self.touch_state.has_velocity() {
                    self.windows.on_drag_release();
                }
            },
            Some(TouchAction::HandleGesture(gesture)) => self.on_handle_gesture(gesture),
            Some(TouchAction::UserGesture((program, args))) => self.on_user_gesture(program, args),
            _ => (),
        }

        self.touch_state.position = position;
    }

    /// Dispatch handle gestures.
    fn on_handle_gesture(&mut self, gesture: HandleGesture) {
        // Process gesture updates.
        if self.touch_state.touching() || self.touch_state.has_velocity() {
            self.windows.on_gesture(&mut self.touch_state, gesture);
        } else {
            self.windows.on_gesture_done(gesture);
            self.touch_state.cancel_velocity();
        }

        // Notify client.
        self.touch_state.touch.cancel();
    }

    /// Dispatch user gestures.
    fn on_user_gesture(&mut self, program: String, args: Vec<String>) {
        // Execute subcommand.
        if let Err(err) = daemon::spawn(&program, &args) {
            error!("Failed gesture command {program} {args:?}: {err}");
        }

        self.touch_state.cancel_velocity();

        // Notify client.
        self.touch_state.touch.cancel();
    }

    /// Process a single velocity tick.
    fn on_velocity_tick(&mut self) -> TimeoutAction {
        // Update velocity and new position.
        //
        // The animations are designed for 60FPS, but should still behave properly for
        // other refresh rates.
        let frame_interval = self.windows.output().frame_interval();
        let velocity = &mut self.touch_state.velocity;
        let position = &mut self.touch_state.position;
        let animation_speed = frame_interval.as_millis() as f64 / 16.;
        velocity.x -= velocity.x.signum()
            * (velocity.x.abs() * FRICTION * animation_speed + 1.).min(velocity.x.abs());
        velocity.y -= velocity.y.signum()
            * (velocity.y.abs() * FRICTION * animation_speed + 1.).min(velocity.y.abs());
        position.x += velocity.x * animation_speed;
        position.y += velocity.y * animation_speed;

        // Generate motion events.
        self.update_position(self.touch_state.position);

        // Ensure updates are rendered.
        self.unstall();

        // Schedule another velocity tick.
        if self.touch_state.has_velocity() {
            TimeoutAction::ToDuration(frame_interval)
        } else {
            TimeoutAction::Drop
        }
    }

    /// Start the velocity timer.
    fn add_velocity_timeout(&mut self) {
        if !self.touch_state.has_velocity() {
            return;
        }

        // Remove old timers.
        if let Some(velocity_timer) = self.touch_state.velocity_timer.take() {
            self.event_loop.remove(velocity_timer);
        }

        // Stage new velocity timer.
        let timer = Timer::from_duration(self.windows.output().frame_interval());
        let velocity_timer = self
            .event_loop
            .insert_source(timer, |_, _, catacomb| catacomb.on_velocity_tick())
            .expect("insert velocity timer");
        self.touch_state.velocity_timer = Some(velocity_timer);
    }

    /// Handle new keyboard input events.
    fn on_keyboard_input<I: InputBackend>(&mut self, event: &impl KeyboardKeyEvent<I>) {
        let keyboard = match self.seat.get_keyboard() {
            Some(keyboard) => keyboard,
            None => return,
        };
        let serial = SERIAL_COUNTER.next_serial();
        let time = Event::time(event) as u32;
        let code = event.key_code();
        let state = event.state();

        // Get desired action for this key.
        let action = keyboard.input(self, code, state, serial, time, |catacomb, mods, keysym| {
            Self::keyboard_action(catacomb, mods, keysym, state)
        });

        // Dispatch input action.
        //
        // This needs to be handled separately since a lock is held across the
        // `keyboard.input` closure blocking `KeyboardHandle::set_focus`.
        match action {
            Some(InputAction::ChangeVt(vt)) => self.backend.change_vt(vt),
            Some(InputAction::ToggleSleep) => self.toggle_sleep(),
            Some(InputAction::PowerDown(id)) => {
                self.button_state.power = Some(id);

                // Open drawer if button is held after timeout while the screen is on.
                if !self.sleeping {
                    let timer = Timer::from_duration(BUTTON_HOLD_DURATION);
                    self.event_loop
                        .insert_source(timer, move |_, _, catacomb| {
                            Self::handle_power_hold(catacomb, id)
                        })
                        .expect("insert power button timer");
                }
            },
            _ => (),
        }
    }

    /// Get keyboard action for a keysym.
    fn keyboard_action(
        catacomb: &mut Catacomb,
        mods: &ModifiersState,
        keysym: KeysymHandle,
        state: KeyState,
    ) -> FilterResult<InputAction> {
        match (keysym.modified_sym(), state) {
            (keysym @ keysyms::KEY_XF86Switch_VT_1..=keysyms::KEY_XF86Switch_VT_12, _) => {
                let vt = (keysym - keysyms::KEY_XF86Switch_VT_1 + 1) as i32;
                InputAction::ChangeVt(vt).into()
            },
            // Filter power buttons pressed during suspend.
            (keysyms::KEY_XF86PowerOff, _)
                if catacomb.last_resume.elapsed() <= RESUME_INHIBIT_DURATION =>
            {
                InputAction::None.into()
            },
            (keysyms::KEY_XF86PowerOff, KeyState::Pressed) => {
                let id = catacomb.button_state.next_id();
                InputAction::PowerDown(id).into()
            },
            (keysyms::KEY_XF86PowerOff, KeyState::Released) => {
                // Toggle screen DPMS status on short press.
                if catacomb.button_state.power.take().is_some() {
                    InputAction::ToggleSleep.into()
                } else {
                    InputAction::None.into()
                }
            },
            (_, KeyState::Released) => {
                // Get raw keysym.
                let raw_keysym = keysym.raw_syms();
                if raw_keysym.len() != 1 {
                    FilterResult::Forward
                } else {
                    Self::handle_user_binding(catacomb, mods, raw_keysym[0])
                }
            },
            _ => FilterResult::Forward,
        }
    }

    /// Handle user-defined key bindings.
    fn handle_user_binding(
        catacomb: &mut Catacomb,
        mods: impl Into<Modifiers>,
        raw_keysym: Keysym,
    ) -> FilterResult<InputAction> {
        let mods = mods.into();

        // Get currently focused app.
        let active_app = catacomb.windows.focus().and_then(|(_, app_id)| app_id);

        // Execute all matching keybindings.
        let mut filter_result = FilterResult::Forward;
        for key_binding in &catacomb.key_bindings {
            if key_binding.key == raw_keysym
                && key_binding.mods == mods
                && key_binding.app_id.matches(active_app.as_ref())
            {
                // Execute subcommand.
                let program = &key_binding.program;
                let arguments = &key_binding.arguments;
                if let Err(err) = daemon::spawn(program, arguments) {
                    error!("Failed keybinding command {program} {arguments:?}: {err}");
                }

                // Prevent key propagation.
                filter_result = InputAction::None.into();
            }
        }

        filter_result
    }

    /// Apply an output transform to a point.
    fn transform_position<I, E>(&self, event: &E) -> Point<f64, Logical>
    where
        E: AbsolutePositionEvent<I>,
        I: InputBackend,
    {
        let canvas = self.windows.canvas();
        let screen_size = canvas.resolution();
        let (mut x, mut y) = event.position_transformed(screen_size).into();
        let (width, height) = screen_size.to_f64().into();

        // Transform X/Y according to output rotation.
        (x, y) = match canvas.orientation() {
            Orientation::Portrait => (x, y),
            Orientation::Landscape => (y, width - x),
            Orientation::InversePortrait => (width - x, height - y),
            Orientation::InverseLandscape => (height - y, x),
        };

        (x, y).into()
    }

    /// Handle long-press of power button.
    fn handle_power_hold(catacomb: &mut Catacomb, button_id: usize) -> TimeoutAction {
        // Ignore timer if button was released or session lock is active.
        if catacomb.button_state.power != Some(button_id) || catacomb.windows.locked() {
            return TimeoutAction::Drop;
        }

        // Reset button state.
        catacomb.button_state.power = None;

        // Always press rumble, to prevent accidental shutdown.
        catacomb.vibrator.vibrate(100, 0, 1);

        // Execute user bindings for `XF86PowerOff`.
        let mods = Modifiers::default();
        Self::handle_user_binding(catacomb, mods, keysyms::KEY_XF86PowerOff);

        TimeoutAction::Drop
    }
}

/// Actions to be taken on keyboard input.
enum InputAction {
    PowerDown(usize),
    ChangeVt(i32),
    ToggleSleep,
    None,
}

impl From<InputAction> for FilterResult<InputAction> {
    fn from(action: InputAction) -> Self {
        FilterResult::Intercept(action)
    }
}
