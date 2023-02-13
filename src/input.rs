//! Input event handling.

use std::time::{Duration, Instant};

use smithay::backend::input::{
    AbsolutePositionEvent, ButtonState, Event, InputBackend, InputEvent, KeyState,
    KeyboardKeyEvent, MouseButton, PointerButtonEvent, TouchEvent as _, TouchSlot,
};
use smithay::input::keyboard::{keysyms, FilterResult};
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{LoopHandle, RegistrationToken};
use smithay::utils::{Logical, Point, Rectangle, SERIAL_COUNTER};
use smithay::wayland::seat::TouchHandle;

use crate::catacomb::Catacomb;
use crate::config::APP_DRAWER;
use crate::orientation::Orientation;
use crate::output::{Output, GESTURE_HANDLE_HEIGHT};
use crate::windows::surface::OffsetSurface;

/// Time before a tap is considered a hold.
pub const HOLD_DURATION: Duration = Duration::from_secs(1);

/// Duration after resume for which power button presses will be ignored.
const RESUME_INHIBIT_DURATION: Duration = Duration::from_millis(500);

/// Time before button press is considered a hold.
const BUTTON_HOLD_DURATION: Duration = Duration::from_millis(500);

/// Maximum distance before touch input is considered a drag.
const MAX_TAP_DISTANCE: f64 = 20.;

/// Friction for velocity computation.
const FRICTION: f64 = 0.1;

/// Duration until suspend after screen is turned off.
const SUSPEND_TIMEOUT: Duration = Duration::from_secs(30);

/// Touch slot for pointer emulation.
///
/// The touch slot `None`, which is usually used for devices that do not support
/// multitouch, does not work properly with some clients like GTK. To work
/// around this we pick an arbitrary multitouch slot instead.
const POINTER_TOUCH_SLOT: Option<u32> = Some(0);

/// Touch input state.
pub struct TouchState {
    pub position: Point<f64, Logical>,

    event_loop: LoopHandle<'static, Catacomb>,
    velocity_timer: Option<RegistrationToken>,
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
    fn start(&mut self, output: &Output, position: Point<f64, Logical>) {
        self.cancel_velocity();

        self.start = TouchStart::new(output, position);
        self.position = position;
        self.is_drag = false;
    }

    /// Check if there's any touch velocity present.
    fn has_velocity(&self) -> bool {
        self.velocity.x.abs() >= f64::EPSILON || self.velocity.y.abs() >= f64::EPSILON
    }

    /// Check if there's currently any touch interaction active.
    #[inline]
    fn touching(&self) -> bool {
        self.slot.is_some()
    }

    /// Get the updated active touch action.
    fn action(&mut self, output: &Output) -> Option<TouchAction> {
        let touching = self.touching();
        if self.start.is_gesture && !touching {
            if let Some(gesture) = Gesture::from_points(output, self.start.position, self.position)
            {
                return Some(TouchAction::Gesture(gesture));
            }
        }

        // Convert to drag as soon as distance/time was exceeded once.
        let delta = self.start.position - self.position;
        if self.is_drag
            || f64::sqrt(delta.x.powi(2) + delta.y.powi(2)) > MAX_TAP_DISTANCE
            || self.start.time.elapsed() >= HOLD_DURATION
        {
            self.is_drag = true;
            return (!self.start.is_gesture).then_some(TouchAction::Drag);
        }

        (!touching).then_some(TouchAction::Tap)
    }
}

/// Start of a touch interaction.
struct TouchStart {
    position: Point<f64, Logical>,
    is_gesture: bool,
    time: Instant,
}

impl Default for TouchStart {
    fn default() -> Self {
        Self { time: Instant::now(), position: Default::default(), is_gesture: Default::default() }
    }
}

impl TouchStart {
    fn new(output: &Output, position: Point<f64, Logical>) -> Self {
        Self { is_gesture: Gesture::is_start(output, position), time: Instant::now(), position }
    }
}

/// Available touch input actions.
#[derive(Debug, Copy, Clone)]
enum TouchAction {
    Gesture(Gesture),
    Drag,
    Tap,
}

/// Touch gestures.
#[derive(Debug, Copy, Clone)]
pub enum Gesture {
    Up,
    Left,
    Right,
}

impl Gesture {
    /// Get a gesture from its start/end.
    fn from_points(
        output: &Output,
        start: Point<f64, Logical>,
        end: Point<f64, Logical>,
    ) -> Option<Self> {
        // Handle left/right drag on gesture handle.
        if Self::is_start(output, end) {
            let delta = end.x - start.x;
            if delta < -MAX_TAP_DISTANCE {
                return Some(Gesture::Left);
            } else if delta > MAX_TAP_DISTANCE {
                return Some(Gesture::Right);
            }
        }

        // Handle drag up.
        let output_size = output.size().to_f64();
        let drag_up_rect =
            Rectangle::from_loc_and_size((0., 0.), (output_size.w, output_size.h * 0.75));
        if drag_up_rect.contains(end) {
            return Some(Gesture::Up);
        }

        None
    }

    /// Check if a touch should start a new gesture.
    fn is_start(output: &Output, position: Point<f64, Logical>) -> bool {
        let output_size = output.size().to_f64();
        let loc = (0., output_size.h - GESTURE_HANDLE_HEIGHT as f64);
        Rectangle::from_loc_and_size(loc, output_size).contains(position)
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
        self.unstall();
    }

    /// Process new input events.
    pub fn handle_input<I: InputBackend>(&mut self, event: InputEvent<I>) {
        // Ignore non-keyboard input events while the screen is off.
        if self.sleeping && !matches!(event, InputEvent::Keyboard { .. }) {
            return;
        }

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
        let surface = self.windows.touch_surface_at(event.position);

        // Notify client.
        if let Some(OffsetSurface { surface, offset }) = surface {
            let serial = SERIAL_COUNTER.next_serial();
            self.touch_state.touch.down(serial, time, &surface, offset, slot, position);
        }

        // Allow only a single touch at a time.
        if self.touch_state.slot.is_some() {
            return;
        }
        self.touch_state.slot = Some(slot);

        // Initialize the touch state.
        self.touch_state.start(self.windows.output(), position);

        // Only send touch start if there's no gesture in progress.
        if !self.touch_state.start.is_gesture {
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

        match self.touch_state.action(self.windows.output()) {
            Some(TouchAction::Tap) => {
                self.windows.on_tap(self.touch_state.position);
            },
            Some(TouchAction::Drag | TouchAction::Gesture(_)) => {
                self.add_velocity_timeout();
                self.update_position(self.touch_state.position);
            },
            None => self.add_velocity_timeout(),
        }
    }

    /// Handle touch input movement.
    fn on_touch_motion(&mut self, event: TouchEvent) {
        // Notify client.
        self.touch_state.touch.motion(event.time, event.slot, event.position);

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
        match self.touch_state.action(self.windows.output()) {
            Some(TouchAction::Drag) => {
                self.windows.on_drag(&mut self.touch_state, position);

                // Signal drag end once no more velocity is present.
                if !self.touch_state.touching() && !self.touch_state.has_velocity() {
                    self.windows.on_drag_release();
                }
            },
            Some(TouchAction::Gesture(gesture)) => self.on_gesture(gesture),
            _ => (),
        }

        self.touch_state.position = position;
    }

    /// Dispatch gestures if it was completed.
    fn on_gesture(&mut self, gesture: Gesture) {
        // Only accept gestures when the touch input was released.
        if self.touch_state.touching() {
            return;
        }

        self.windows.on_gesture(gesture);
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
        let velocity = &mut self.touch_state.velocity;
        let position = &mut self.touch_state.position;
        let animation_speed = self.windows.output().frame_interval().as_millis() as f64 / 16.;
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
            TimeoutAction::ToDuration(self.windows.output().frame_interval())
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
        let keycode = event.key_code();
        let state = event.state();

        keyboard.input(self, keycode, state, serial, time, |catacomb, _mods, keysym| {
            match (keysym.modified_sym(), state) {
                (keysym @ keysyms::KEY_XF86Switch_VT_1..=keysyms::KEY_XF86Switch_VT_12, _) => {
                    let vt = (keysym - keysyms::KEY_XF86Switch_VT_1 + 1) as i32;
                    catacomb.backend.change_vt(vt);
                },
                // Filter power buttons pressed during suspend.
                (keysyms::KEY_XF86PowerOff, _)
                    if catacomb.last_resume.elapsed() <= RESUME_INHIBIT_DURATION => {},
                (keysyms::KEY_XF86PowerOff, KeyState::Pressed) => {
                    let id = catacomb.button_state.next_id();
                    catacomb.button_state.power = Some(id);

                    // Open drawer if button is still held after timeout.
                    let timer = Timer::from_duration(BUTTON_HOLD_DURATION);
                    catacomb
                        .event_loop
                        .insert_source(timer, move |_, _, catacomb| {
                            // Ignore timer if button was released.
                            if catacomb.button_state.power != Some(id) {
                                return TimeoutAction::Drop;
                            }

                            // Reset button state.
                            catacomb.button_state.power = None;

                            // Open drawer.
                            if let Err(err) = crate::daemon(APP_DRAWER, []) {
                                eprintln!("Unable to launch {APP_DRAWER:?}: {err}");
                            }

                            TimeoutAction::Drop
                        })
                        .expect("insert power button timer");
                },
                (keysyms::KEY_XF86PowerOff, KeyState::Released) => {
                    // Toggle screen DPMS status on short press.
                    if catacomb.button_state.power.take().is_some() {
                        catacomb.toggle_sleep();

                        // Remove timer on wakeup or if it was already staged.
                        if let Some(suspend_timer) = catacomb.suspend_timer.take() {
                            catacomb.event_loop.remove(suspend_timer);
                        }

                        // Timeout after prolonged inactivity.
                        if catacomb.sleeping {
                            let timer = Timer::from_duration(SUSPEND_TIMEOUT);
                            catacomb.suspend_timer = Some(
                                catacomb
                                    .event_loop
                                    .insert_source(timer, |_, _, catacomb| {
                                        catacomb.suspend();
                                        TimeoutAction::Drop
                                    })
                                    .expect("insert suspend timer"),
                            );
                        }
                    }
                },
                _ => return FilterResult::Forward,
            }

            FilterResult::Intercept(())
        });
    }

    /// Apply an output transform to a point.
    fn transform_position<I, E>(&self, event: &E) -> Point<f64, Logical>
    where
        E: AbsolutePositionEvent<I>,
        I: InputBackend,
    {
        let screen_size = self.windows.output().resolution();
        let (mut x, mut y) = event.position_transformed(screen_size).into();
        let (width, height) = screen_size.to_f64().into();

        // Transform X/Y according to output rotation.
        (x, y) = match self.windows.output().orientation() {
            Orientation::Portrait => (x, y),
            Orientation::Landscape => (y, width - x),
            Orientation::InversePortrait => (width - x, height - y),
            Orientation::InverseLandscape => (height - y, x),
        };

        (x, y).into()
    }
}
