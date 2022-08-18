//! Input event handling.

use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use calloop::timer::{Timer, TimerHandle};
use calloop::LoopHandle;
use smithay::backend::input::{
    ButtonState, Event, InputBackend, InputEvent, KeyState, KeyboardKeyEvent, MouseButton,
    PointerButtonEvent, PositionEvent, TouchEvent as _, TouchSlot,
};
#[cfg(feature = "winit")]
use smithay::backend::winit::WinitEvent;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::seat::{keysyms, FilterResult, TouchHandle};
use smithay::wayland::SERIAL_COUNTER;

use crate::catacomb::{Backend, Catacomb};
use crate::config::APP_DRAWER;
use crate::orientation::Orientation;
use crate::output::Output;
use crate::window::OffsetSurface;

/// Time before a tap is considered a hold.
pub const HOLD_DURATION: Duration = Duration::from_secs(1);

/// Time before button press is considered a hold.
const BUTTON_HOLD_DURATION: Duration = Duration::from_millis(500);

/// Accepted overview gesture deviation in pixels at scale 1.
const OVERVIEW_GESTURE_ACCURACY: f64 = 60.;

/// Accepted home gesture deviation in pixels at scale 1.
const HOME_GESTURE_ACCURACY: f64 = 30.;

/// Home gesture distance from the output edges.
const HOME_WIDTH_PERCENTAGE: f64 = 0.25;

/// Maximum distance before touch input is considered a drag.
const MAX_TAP_DISTANCE: f64 = 20.;

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
    pub position: Point<f64, Logical>,
    slot: Option<TouchSlot>,
    velocity: Point<f64, Logical>,
    events: Vec<TouchEvent>,
    timer: TimerHandle<()>,
    touch: TouchHandle,
    start: TouchStart,
    is_drag: bool,
}

impl TouchState {
    pub fn new<B: Backend>(loop_handle: &LoopHandle<'_, Catacomb<B>>, touch: TouchHandle) -> Self {
        let timer = Timer::new().expect("create input timer");
        let timer_handle = timer.handle();
        loop_handle
            .insert_source(timer, |_, _, catacomb| catacomb.on_velocity_tick())
            .expect("insert input timer");

        Self {
            timer: timer_handle,
            touch,
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
        self.velocity = Default::default();
        self.timer.cancel_all_timeouts();
    }

    /// Start a new touch session.
    fn start(&mut self, output: &Output, position: Point<f64, Logical>) {
        self.start = TouchStart::new(output, position);
        self.velocity = Default::default();
        self.timer.cancel_all_timeouts();
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
    fn action(&mut self, output: &Output, overview_active: bool) -> Option<TouchAction> {
        let output_size = output.size().to_f64();
        let touching = self.touching();
        match self.start.gesture {
            Some(Gesture::Overview) if overview_active => (),
            Some(gesture) => {
                if !touching && gesture.end_rect(output_size).contains(self.position) {
                    return Some(TouchAction::Gesture(gesture));
                }
            },
            _ => (),
        }

        // Convert to drag as soon as distance/time was exceeded once.
        let delta = self.start.position - self.position;
        if self.is_drag
            || f64::sqrt(delta.x.powi(2) + delta.y.powi(2)) > MAX_TAP_DISTANCE
            || self.start.time.elapsed() >= HOLD_DURATION
        {
            self.is_drag = true;
            return self.start.gesture.is_none().then(|| TouchAction::Drag);
        }

        (!touching).then(|| TouchAction::Tap)
    }
}

/// Start of a touch interaction.
struct TouchStart {
    position: Point<f64, Logical>,
    gesture: Option<Gesture>,
    time: Instant,
}

impl Default for TouchStart {
    fn default() -> Self {
        Self { time: Instant::now(), position: Default::default(), gesture: Default::default() }
    }
}

impl TouchStart {
    fn new(output: &Output, position: Point<f64, Logical>) -> Self {
        Self { gesture: Gesture::from_start(output, position), time: Instant::now(), position }
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
    Overview,
    Home,
}

impl Gesture {
    /// Create a gesture from its starting location.
    fn from_start(output: &Output, position: Point<f64, Logical>) -> Option<Self> {
        let match_gesture =
            |gesture: Gesture| gesture.start_rect(output).contains(position).then(|| gesture);
        match_gesture(Gesture::Overview).or_else(|| match_gesture(Gesture::Home))
    }

    /// Touch area expected for gesture initiation.
    fn start_rect(&self, output: &Output) -> Rectangle<f64, Logical> {
        let output_size = output.size().to_f64();
        match self {
            Gesture::Overview => {
                let accuracy = OVERVIEW_GESTURE_ACCURACY / output.scale();
                let loc = (output_size.w - accuracy, output_size.h - accuracy);
                Rectangle::from_loc_and_size(loc, output_size)
            },
            Gesture::Home => {
                let accuracy = HOME_GESTURE_ACCURACY / output.scale();
                let loc = (output_size.w * HOME_WIDTH_PERCENTAGE, output_size.h - accuracy);
                let size = (output_size.w - 2. * loc.0, output_size.h);
                Rectangle::from_loc_and_size(loc, size)
            },
        }
    }

    /// Touch area expected for gesture completion.
    fn end_rect(&self, output_size: Size<f64, Logical>) -> Rectangle<f64, Logical> {
        let size = match self {
            Gesture::Overview => (output_size.w * 0.75, output_size.h * 0.75),
            Gesture::Home => (output_size.w, output_size.h * 0.75),
        };
        Rectangle::from_loc_and_size((0., 0.), size)
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

impl<B: Backend> Catacomb<B> {
    /// Process device orientation changes.
    pub fn handle_orientation(&mut self, orientation: Orientation) {
        self.output.set_orientation(orientation);
        self.windows.update_orientation(&mut self.output);
    }

    /// Process winit-specific input events.
    #[cfg(feature = "winit")]
    pub fn handle_winit_input(&mut self, event: WinitEvent) {
        match event {
            // Toggle between portrait/landscape based on window size.
            WinitEvent::Resized { size, .. } => {
                self.output.resize(size);
                self.windows.resize_all(&mut self.output);
            },
            WinitEvent::Input(event) => self.handle_input(event),
            _ => (),
        }
    }

    /// Process new input events.
    pub fn handle_input<I: InputBackend>(&mut self, event: InputEvent<I>) {
        match event {
            InputEvent::Keyboard { event, .. } => self.on_keyboard_input(event),
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
                let event = TouchEvent::new(event_type, event.slot(), event.time(), position);
                self.touch_state.events.push(event);
            },
            InputEvent::TouchUp { event } => {
                let position = self.touch_state.position;
                let event_type = TouchEventType::Up;
                let event = TouchEvent::new(event_type, event.slot(), event.time(), position);
                self.touch_state.events.push(event);
            },
            InputEvent::TouchMotion { event } => {
                let position = self.transform_position(&event);
                let event_type = TouchEventType::Motion;
                let event = TouchEvent::new(event_type, event.slot(), event.time(), position);
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
        };
    }

    /// Handle new touch input start.
    fn on_touch_down(&mut self, event: TouchEvent) {
        let TouchEvent { time, slot, position, .. } = event;
        let surface = self.windows.touch_surface_at(event.position);

        // Notify client.
        let mut layer_touched = false;
        if let Some(OffsetSurface { surface, offset, is_layer }) = surface {
            layer_touched = is_layer;

            let serial = SERIAL_COUNTER.next_serial();
            self.touch_state.touch.down(serial, time, &surface, offset, slot, position);
        }

        // Allow only a single touch at a time.
        if self.touch_state.slot.is_some() {
            return;
        }
        self.touch_state.slot = Some(slot);

        // Initialize the touch state.
        self.touch_state.start(&self.output, position);

        // Inhibit gestures started above layer shell surfaces.
        if layer_touched {
            self.touch_state.start.gesture = None;
        }

        // Only send touch start if there's no gesture in progress.
        if self.touch_state.start.gesture.is_none() {
            self.windows.on_touch_start(&self.output, position);
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

        let overview_active = self.windows.overview_active();
        match self.touch_state.action(&self.output, overview_active) {
            Some(TouchAction::Tap) => {
                self.windows.on_tap(&self.output, self.touch_state.position);
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
        let overview_active = self.windows.overview_active();
        match self.touch_state.action(&self.output, overview_active) {
            Some(TouchAction::Drag) => {
                self.windows.on_drag(&self.output, &mut self.touch_state, position);

                // Signal drag end once no more velocity is present.
                if !self.touch_state.touching() && !self.touch_state.has_velocity() {
                    self.windows.on_drag_release(&self.output);
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

        self.windows.on_gesture(&self.output, gesture);
        self.touch_state.timer.cancel_all_timeouts();

        // Notify client.
        self.touch_state.touch.cancel();
    }

    /// Process a single velocity tick.
    fn on_velocity_tick(&mut self) {
        // Update velocity and new position.
        //
        // The animations are designed for 60FPS, but should still behave properly for
        // other refresh rates.
        let velocity = &mut self.touch_state.velocity;
        let position = &mut self.touch_state.position;
        let animation_speed = self.output.frame_interval() as f64 / 16.;
        velocity.x -= velocity.x.signum()
            * (velocity.x.abs() * FRICTION * animation_speed + 1.).min(velocity.x.abs());
        velocity.y -= velocity.y.signum()
            * (velocity.y.abs() * FRICTION * animation_speed + 1.).min(velocity.y.abs());
        *position += *velocity * animation_speed;

        // Request another callback.
        self.add_velocity_timeout();

        // Generate motion events.
        self.update_position(self.touch_state.position);
    }

    /// Request a new velocity timer callback.
    fn add_velocity_timeout(&self) {
        if self.touch_state.has_velocity() {
            self.touch_state
                .timer
                .add_timeout(Duration::from_millis(self.output.frame_interval()), ());
        }
    }

    /// Handle new keyboard input events.
    fn on_keyboard_input<I: InputBackend>(&mut self, event: impl KeyboardKeyEvent<I>) {
        let serial = SERIAL_COUNTER.next_serial();
        let time = Event::time(&event);
        let keycode = event.key_code();
        let state = event.state();

        self.keyboard.input(keycode, state, serial, time, |_modifiers, keysym| {
            match (keysym.modified_sym(), state) {
                (keysym @ keysyms::KEY_XF86Switch_VT_1..=keysyms::KEY_XF86Switch_VT_12, _) => {
                    let vt = (keysym - keysyms::KEY_XF86Switch_VT_1 + 1) as i32;
                    self.backend.change_vt(vt);
                },
                (keysyms::KEY_XF86PowerOff, KeyState::Pressed) => {
                    let id = self.button_state.next_id();
                    self.button_state.power = Some(id);

                    // Stage timer until hold deadline.
                    let timer = Timer::new().expect("create power button timer");
                    timer.handle().add_timeout(BUTTON_HOLD_DURATION, id);

                    // Open drawer if button is still held after timeout.
                    self.loop_handle
                        .insert_source(timer, |id, _, catacomb| {
                            // Ignore timer if button was released.
                            if catacomb.button_state.power != Some(id) {
                                return;
                            }

                            // Reset button state.
                            catacomb.button_state.power = None;

                            // Open drawer.
                            let _ = Command::new(APP_DRAWER)
                                .stdin(Stdio::null())
                                .stdout(Stdio::null())
                                .stderr(Stdio::null())
                                .spawn();
                        })
                        .expect("insert power button timer");
                },
                (keysyms::KEY_XF86PowerOff, KeyState::Released) => {
                    // Turn off screen on short press.
                    if self.button_state.power.take().is_some() {
                        self.sleeping = !self.sleeping;
                        self.backend.set_sleep(self.sleeping);
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
        E: PositionEvent<I>,
        I: InputBackend,
    {
        let screen_size = self.output.resolution();
        let (mut x, mut y) = event.position_transformed(screen_size).into();
        let (width, height) = screen_size.to_f64().into();

        // Transform X/Y according to output rotation.
        (x, y) = match self.output.orientation() {
            Orientation::Portrait => (x, y),
            Orientation::Landscape => (y, width - x),
            Orientation::InversePortrait => (width - x, height - y),
            Orientation::InverseLandscape => (height - y, x),
        };

        (x, y).into()
    }
}
