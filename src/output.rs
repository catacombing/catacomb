//! Output region handling.

use std::ops::{Add, Deref, Sub};
use std::time::Duration;

use smithay::output::{
    Mode, Output as SmithayOutput, OutputModeSource, PhysicalProperties, Scale, Subpixel,
};
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
const SCALE: f64 = 2.;

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
        let output = SmithayOutput::new(name.into(), properties);
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

    /// Update the output's scale factor.
    pub fn set_scale(&mut self, mut scale: f64) {
        // Ensure scale factor is divisible by 120, to support wp_fractional_scale.
        scale = (scale * 120.).round() / 120.;

        // Update the scale.
        self.canvas.scale = scale;

        // Update the Wayland output.
        self.set_mode(self.canvas.mode);
    }

    /// Update the output's active mode.
    pub fn set_mode(&mut self, mode: Mode) {
        let scale = Some(Scale::Fractional(self.scale()));
        let transform = Some(self.orientation.surface_transform());
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
        Rectangle::new(available.loc, size)
    }

    /// Secondary window dimensions.
    pub fn secondary_rectangle(&self) -> Rectangle<i32, Logical> {
        let available = self.available();
        let mut loc = available.loc;
        if available.size.h > available.size.w {
            let size: Size<i32, Logical> = (available.size.w, available.size.h / 2).into();
            loc.y += (available.size.h + 1) / 2;
            Rectangle::new(loc, size)
        } else {
            let size: Size<i32, Logical> = (available.size.w / 2, available.size.h).into();
            loc.x += (available.size.w + 1) / 2;
            Rectangle::new(loc, size)
        }
    }

    /// Duration between frames.
    pub fn frame_interval(&self) -> Duration {
        Duration::from_nanos(1_000_000_000_000 / self.canvas.mode.refresh as u64)
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

    /// Get the underlying smithay output.
    pub fn smithay_output(&self) -> &SmithayOutput {
        &self.output
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
    scale: f64,
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

    /// Output device resolution in physical coordinates.
    pub fn physical_resolution(&self) -> Size<i32, Physical> {
        self.mode.size
    }

    /// Output device size with transformations applied.
    pub fn physical_size(&self) -> Size<i32, Physical> {
        let (w, h) = self.physical_resolution().into();
        match self.orientation {
            Orientation::Portrait | Orientation::InversePortrait => (w, h).into(),
            Orientation::Landscape | Orientation::InverseLandscape => (h, w).into(),
        }
    }

    /// Output device resolution.
    ///
    /// This represents the size of the display before applying any
    /// transformations.
    pub fn resolution(&self) -> Size<i32, Logical> {
        self.physical_resolution().to_f64().to_logical(self.scale()).to_i32_round()
    }

    /// Output size.
    ///
    /// Output size with all transformations applied.
    pub fn size(&self) -> Size<i32, Logical> {
        self.physical_size().to_f64().to_logical(self.scale()).to_i32_round()
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
        Rectangle::new(loc.into(), size)
    }

    /// Area of the output available in the application overview.
    ///
    /// This excludes foreground layer shell windows, since these are hidden
    /// while the overview is active.
    pub fn available_overview(&self) -> Rectangle<i32, Logical> {
        let reserved = |exclusivity| match exclusivity {
            Exclusivity::Top(_) | Exclusivity::Overlay(_) => 0,
            Exclusivity::Background(reserved) | Exclusivity::Bottom(reserved) => reserved,
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
        Rectangle::new(loc.into(), size)
    }

    /// Area of the output available for fullscreen surfaces.
    ///
    /// This only accounts for overlay layer shell windows, since these are the
    /// only ones visible in fullscreen mode.
    pub fn available_fullscreen(&self) -> Rectangle<i32, Logical> {
        let reserved = |exclusivity| match exclusivity {
            Exclusivity::Overlay(reserved) => reserved,
            _ => 0,
        };

        // Get reserved space on the overlay layer.
        let (top, right, bottom, left) = (
            reserved(self.exclusive.top),
            reserved(self.exclusive.right),
            reserved(self.exclusive.bottom),
            reserved(self.exclusive.left),
        );

        let loc = (left, top);
        let mut size = self.size();
        size.w -= left + right;
        size.h -= top + bottom;
        Rectangle::new(loc.into(), size)
    }

    /// Output fractional scale.
    pub fn scale(&self) -> f64 {
        self.scale
    }
}

impl From<&Canvas> for OutputModeSource {
    fn from(canvas: &Canvas) -> Self {
        Self::Static {
            size: canvas.physical_resolution(),
            scale: canvas.scale().into(),
            transform: canvas.orientation().surface_transform(),
        }
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
    Overlay(i32),
    Top(i32),
    Background(i32),
    Bottom(i32),
}

impl Default for Exclusivity {
    fn default() -> Self {
        Self::Background(0)
    }
}

impl Exclusivity {
    fn from_layer(layer: Layer, reserved: i32) -> Self {
        match layer {
            Layer::Bottom => Self::Bottom(reserved),
            Layer::Background => Self::Background(reserved),
            Layer::Top => Self::Top(reserved),
            Layer::Overlay => Self::Overlay(reserved),
        }
    }
}

impl Add<Exclusivity> for i32 {
    type Output = i32;

    fn add(self, exclusivity: Exclusivity) -> Self::Output {
        match exclusivity {
            Exclusivity::Bottom(reserved)
            | Exclusivity::Background(reserved)
            | Exclusivity::Top(reserved)
            | Exclusivity::Overlay(reserved) => self + reserved,
        }
    }
}

impl Sub<Exclusivity> for i32 {
    type Output = i32;

    fn sub(self, exclusivity: Exclusivity) -> Self::Output {
        match exclusivity {
            Exclusivity::Bottom(reserved)
            | Exclusivity::Background(reserved)
            | Exclusivity::Top(reserved)
            | Exclusivity::Overlay(reserved) => self - reserved,
        }
    }
}

impl Deref for Exclusivity {
    type Target = i32;

    fn deref(&self) -> &Self::Target {
        match self {
            Exclusivity::Bottom(reserved)
            | Exclusivity::Background(reserved)
            | Exclusivity::Top(reserved)
            | Exclusivity::Overlay(reserved) => reserved,
        }
    }
}
