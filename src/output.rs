//! Output region handling.

use std::time::Duration;

use smithay::reexports::wayland_server::backend::GlobalId;
use smithay::reexports::wayland_server::protocol::wl_output::Subpixel;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{Logical, Physical, Rectangle, Size, Transform};
use smithay::wayland::output::{Mode, Output as SmithayOutput, PhysicalProperties, Scale};
use smithay::wayland::shell::wlr_layer::{Anchor, ExclusiveZone};

use crate::catacomb::Catacomb;
use crate::orientation::Orientation;

/// Use a fixed output scale.
const SCALE: i32 = 2;

/// Wayland output, typically a screen.
pub struct Output {
    /// Layer shell reserved space.
    pub exclusive: ExclusiveSpace,

    global: Option<GlobalId>,
    orientation: Orientation,
    display: DisplayHandle,
    output: SmithayOutput,
    transform: Transform,
    scale: i32,
    mode: Mode,
}

impl Output {
    pub fn new<B: 'static>(
        display: &DisplayHandle,
        name: impl Into<String>,
        mode: Mode,
        properties: PhysicalProperties,
    ) -> Self {
        let output = SmithayOutput::new(name.into(), properties, None);
        let global = Some(output.create_global::<Catacomb<B>>(display));
        let scale = SCALE;

        let mut output = Self {
            global,
            output,
            scale,
            mode,
            display: display.clone(),
            orientation: Default::default(),
            transform: Default::default(),
            exclusive: Default::default(),
        };

        // Update the active mode.
        output.set_mode(output.mode);

        output
    }

    /// Create a new dummy output.
    pub fn new_dummy<B: 'static>(display: &DisplayHandle) -> Self {
        let mode = Mode { size: (0, 0).into(), refresh: 0 };
        Output::new::<B>(display, "dummy-0", mode, PhysicalProperties {
            subpixel: Subpixel::Unknown,
            model: "dummy-0".into(),
            make: "dummy-0".into(),
            size: (0, 0).into(),
        })
    }

    // TODO: Remove when nuking winit.
    //
    /// Destroy this output's gobal.
    pub fn destroy<B: 'static>(mut self) {
        if let Some(global) = self.global.take() {
            self.display.remove_global::<Catacomb<B>>(global);
        }
    }

    /// Update the output's active mode.
    #[inline]
    pub fn set_mode(&mut self, mode: Mode) {
        let scale = Some(Scale::Integer(self.scale));
        let transform = Some(self.transform.into());
        self.output.change_current_state(Some(mode), transform, scale, None);
        self.output.set_preferred(mode);
        self.mode = mode;
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

    /// Area of the output not reserved for layer shell windows.
    pub fn available(&self) -> Rectangle<i32, Logical> {
        let loc = (self.exclusive.left, self.exclusive.top);
        let mut size = self.size();
        size.w -= self.exclusive.left + self.exclusive.right;
        size.h -= self.exclusive.top + self.exclusive.bottom;
        Rectangle::from_loc_and_size(loc, size)
    }

    /// Resize the output.
    #[cfg(feature = "winit")]
    pub fn resize(&mut self, size: Size<i32, Physical>) {
        self.mode.size = size;
    }

    /// Duration between frames in milliseconds.
    pub fn frame_interval(&self) -> Duration {
        Duration::from_millis(1_000_000 / self.mode.refresh as u64)
    }

    /// Update the device orientation.
    pub fn set_orientation(&mut self, orientation: Orientation) {
        self.transform = orientation.transform();
        self.orientation = orientation;

        // Update output mode to apply transform.
        self.set_mode(self.mode);
    }

    /// Device orientation.
    pub fn orientation(&self) -> Orientation {
        self.orientation
    }

    /// Output scale.
    pub fn scale(&self) -> i32 {
        self.scale
    }

    /// Add the given surface to the display.
    pub fn enter(&self, surface: &WlSurface) {
        self.output.enter(&self.display, surface);
    }

    /// Remove the given surface from the display.
    pub fn leave(&self, surface: &WlSurface) {
        self.output.leave(&self.display, surface);
    }
}

/// Output space reserved by layer shell surfaces.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct ExclusiveSpace {
    pub top: i32,
    pub right: i32,
    pub bottom: i32,
    pub left: i32,
}

impl ExclusiveSpace {
    /// Update the exclusive space.
    pub fn update(&mut self, anchor: Anchor, zone: ExclusiveZone) {
        let reserved = match zone {
            ExclusiveZone::Exclusive(reserved) => reserved,
            _ => return,
        };

        if anchor == Anchor::TOP || anchor == !Anchor::BOTTOM {
            self.top = reserved as i32;
        } else if anchor == Anchor::LEFT || anchor == !Anchor::RIGHT {
            self.left = reserved as i32;
        } else if anchor == Anchor::BOTTOM || anchor == !Anchor::TOP {
            self.bottom = reserved as i32;
        } else if anchor == Anchor::RIGHT || anchor == !Anchor::LEFT {
            self.right = reserved as i32;
        }
    }

    /// Revert the exclusive zone created with the supplied parameters.
    pub fn reset(&mut self, anchor: Anchor, zone: ExclusiveZone) {
        if let ExclusiveZone::Exclusive(_) = zone {
            self.update(anchor, ExclusiveZone::Exclusive(0));
        }
    }
}
