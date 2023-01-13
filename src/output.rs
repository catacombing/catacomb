//! Output region handling.

use std::ops::{Add, Deref, Sub};
use std::time::Duration;

use smithay::output::{Mode, Output as SmithayOutput, PhysicalProperties, Scale, Subpixel};
use smithay::reexports::wayland_server::backend::GlobalId;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{Logical, Physical, Rectangle, Size};
use smithay::wayland::shell::wlr_layer::{Anchor, ExclusiveZone, Layer};

use crate::catacomb::Catacomb;
use crate::orientation::Orientation;

/// Height at bottom of the screen reserved for gestures at scale factor 1.
pub const GESTURE_HANDLE_HEIGHT: i32 = 15;

/// Use a fixed output scale.
const SCALE: i32 = 2;

/// Wayland output, typically a screen.
#[derive(Debug)]
pub struct Output {
    global: Option<GlobalId>,
    display: DisplayHandle,
    output: SmithayOutput,
    canvas: Canvas,
}

impl Drop for Output {
    fn drop(&mut self) {
        // Destroy output's global, to remove it from clients.
        if let Some(global) = self.global.take() {
            self.display.remove_global::<Catacomb>(global);
        }
    }
}

impl Output {
    pub fn new(
        display: &DisplayHandle,
        name: impl Into<String>,
        mode: Mode,
        properties: PhysicalProperties,
    ) -> Self {
        let output = SmithayOutput::new(name.into(), properties, None);
        let global = Some(output.create_global::<Catacomb>(display));

        let mut output =
            Self { global, output, canvas: Canvas::new(mode), display: display.clone() };

        // Update the active mode.
        output.set_mode(output.mode);

        output
    }

    /// Create a new dummy output.
    pub fn new_dummy(display: &DisplayHandle) -> Self {
        let mode = Mode { size: (0, 0).into(), refresh: 0 };
        Output::new(display, "dummy-0", mode, PhysicalProperties {
            subpixel: Subpixel::Unknown,
            model: "dummy-0".into(),
            make: "dummy-0".into(),
            size: (0, 0).into(),
        })
    }

    /// Update the output's active mode.
    #[inline]
    pub fn set_mode(&mut self, mode: Mode) {
        let scale = Some(Scale::Integer(self.canvas.scale));
        let transform = Some(self.orientation.output_transform());
        self.output.change_current_state(Some(mode), transform, scale, None);
        self.output.set_preferred(mode);
        self.canvas.mode = mode;
    }

    /// Primary window dimensions.
    pub fn primary_rectangle(&self, secondary_visible: bool) -> Rectangle<i32, Logical> {
        let available = self.available();
        let size: Size<i32, Logical> = if available.size.h > available.size.w && secondary_visible {
            (available.size.w, (available.size.h + 1) / 2).into()
        } else if available.size.w > available.size.h && secondary_visible {
            ((available.size.w + 1) / 2, available.size.h).into()
        } else {
            (available.size.w, available.size.h).into()
        };
        Rectangle::from_loc_and_size(available.loc, size)
    }

    /// Secondary window dimensions.
    pub fn secondary_rectangle(&self) -> Rectangle<i32, Logical> {
        let available = self.available();
        let mut loc = available.loc;
        if available.size.h > available.size.w {
            let size: Size<i32, Logical> = (available.size.w, available.size.h / 2).into();
            loc.y += (available.size.h + 1) / 2;
            Rectangle::from_loc_and_size(loc, size)
        } else {
            let size: Size<i32, Logical> = (available.size.w / 2, available.size.h).into();
            loc.x += (available.size.w + 1) / 2;
            Rectangle::from_loc_and_size(loc, size)
        }
    }

    /// Duration between frames in milliseconds.
    pub fn frame_interval(&self) -> Duration {
        Duration::from_millis(1_000_000 / self.canvas.mode.refresh as u64)
    }

    /// Update the device orientation.
    pub fn set_orientation(&mut self, orientation: Orientation) {
        self.canvas.orientation = orientation;

        // Update output mode to apply transform.
        self.set_mode(self.canvas.mode);
    }

    /// Add the given surface to the display.
    pub fn enter(&self, surface: &WlSurface) {
        self.output.enter(surface);
    }

    /// Remove the given surface from the display.
    pub fn leave(&self, surface: &WlSurface) {
        self.output.leave(surface);
    }

    /// Get output state.
    pub fn canvas(&self) -> &Canvas {
        &self.canvas
    }

    /// Get a mutable reference to the output's reserved space.
    pub fn exclusive(&mut self) -> &mut ExclusiveSpace {
        &mut self.canvas.exclusive
    }
}

impl Deref for Output {
    type Target = Canvas;

    fn deref(&self) -> &Self::Target {
        &self.canvas
    }
}

/// Output state for rendering.
#[derive(Copy, Clone, Debug)]
pub struct Canvas {
    exclusive: ExclusiveSpace,
    orientation: Orientation,
    scale: i32,
    mode: Mode,
}

impl Canvas {
    fn new(mode: Mode) -> Self {
        let scale = SCALE;
        Self { mode, scale, orientation: Default::default(), exclusive: Default::default() }
    }

    /// Device orientation.
    pub fn orientation(&self) -> Orientation {
        self.orientation
    }

    /// Output device resolution.
    ///
    /// This represents the size of the display before applying any
    /// transformations.
    pub fn resolution(&self) -> Size<i32, Logical> {
        self.mode.size.to_logical(self.scale)
    }

    /// Output device resolution in physical coordinates.
    pub fn physical_resolution(&self) -> Size<i32, Physical> {
        self.mode.size
    }

    /// Output size.
    ///
    /// Output size with all transformations applied.
    pub fn size(&self) -> Size<i32, Logical> {
        let (w, h) = self.resolution().into();
        match self.orientation {
            Orientation::Portrait | Orientation::InversePortrait => (w, h).into(),
            Orientation::Landscape | Orientation::InverseLandscape => (h, w).into(),
        }
    }

    /// Size available for windowing.
    ///
    /// This is the size available for window placement, excluding area reserved
    /// for compositor controls.
    pub fn wm_size(&self) -> Size<i32, Logical> {
        let mut size = self.size();
        size.h -= GESTURE_HANDLE_HEIGHT;
        size
    }

    /// Area of the output not reserved for layer shell windows.
    pub fn available(&self) -> Rectangle<i32, Logical> {
        let loc = (*self.exclusive.left, *self.exclusive.top);
        let mut size = self.wm_size();
        size.w -= *self.exclusive.left + self.exclusive.right;
        size.h -= *self.exclusive.top + self.exclusive.bottom;
        Rectangle::from_loc_and_size(loc, size)
    }

    /// Area of the output available in the application overview.
    ///
    /// This excludes foreground layer shell windows, since these are hidden
    /// while the overview is active.
    pub fn available_overview(&self) -> Rectangle<i32, Logical> {
        let reserved = |exclusivity| match exclusivity {
            Exclusivity::Foreground(_) => 0,
            Exclusivity::Background(reserved) => reserved,
        };

        // Get reserved space on the background layers.
        let (top, right, bottom, left) = (
            reserved(self.exclusive.top),
            reserved(self.exclusive.right),
            reserved(self.exclusive.bottom),
            reserved(self.exclusive.left),
        );

        let loc = (left, top);
        let mut size = self.wm_size();
        size.w -= left + right;
        size.h -= top + bottom;
        Rectangle::from_loc_and_size(loc, size)
    }

    /// Output scale.
    pub fn scale(&self) -> i32 {
        self.scale
    }
}

/// Output space reserved by layer shell surfaces.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct ExclusiveSpace {
    pub top: Exclusivity,
    pub right: Exclusivity,
    pub bottom: Exclusivity,
    pub left: Exclusivity,
}

impl ExclusiveSpace {
    /// Update the exclusive space.
    pub fn update(&mut self, anchor: Anchor, zone: ExclusiveZone, layer: Layer) {
        let reserved = match zone {
            ExclusiveZone::Exclusive(reserved) => reserved,
            _ => return,
        };

        if anchor == Anchor::TOP || anchor == !Anchor::BOTTOM {
            self.top = Exclusivity::from_layer(layer, reserved as i32);
        } else if anchor == Anchor::LEFT || anchor == !Anchor::RIGHT {
            self.left = Exclusivity::from_layer(layer, reserved as i32);
        } else if anchor == Anchor::BOTTOM || anchor == !Anchor::TOP {
            self.bottom = Exclusivity::from_layer(layer, reserved as i32);
        } else if anchor == Anchor::RIGHT || anchor == !Anchor::LEFT {
            self.right = Exclusivity::from_layer(layer, reserved as i32);
        }
    }

    /// Revert the exclusive zone created with the supplied parameters.
    pub fn reset(&mut self, anchor: Anchor, zone: ExclusiveZone) {
        if let ExclusiveZone::Exclusive(_) = zone {
            self.update(anchor, ExclusiveZone::Exclusive(0), Layer::Bottom);
        }
    }
}

/// Type of exclusive space reservation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Exclusivity {
    Foreground(i32),
    Background(i32),
}

impl Default for Exclusivity {
    fn default() -> Self {
        Self::Background(0)
    }
}

impl Exclusivity {
    fn from_layer(layer: Layer, reserved: i32) -> Self {
        match layer {
            Layer::Bottom | Layer::Background => Self::Background(reserved),
            Layer::Top | Layer::Overlay => Self::Foreground(reserved),
        }
    }
}

impl Add<Exclusivity> for i32 {
    type Output = i32;

    fn add(self, exclusivity: Exclusivity) -> Self::Output {
        match exclusivity {
            Exclusivity::Background(reserved) | Exclusivity::Foreground(reserved) => {
                self + reserved
            },
        }
    }
}

impl Sub<Exclusivity> for i32 {
    type Output = i32;

    fn sub(self, exclusivity: Exclusivity) -> Self::Output {
        match exclusivity {
            Exclusivity::Background(reserved) | Exclusivity::Foreground(reserved) => {
                self - reserved
            },
        }
    }
}

impl Deref for Exclusivity {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        match self {
            Exclusivity::Background(reserved) | Exclusivity::Foreground(reserved) => reserved,
        }
    }
}
