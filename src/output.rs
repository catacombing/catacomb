//! Output region handling.

use std::ops::Deref;

use smithay::reexports::wayland_server::protocol::wl_output::{Subpixel, WlOutput};
use smithay::reexports::wayland_server::{Display, Global};
use smithay::utils::{Logical, Physical, Rectangle, Size};
use smithay::wayland::output::{Mode, Output as SmithayOutput, PhysicalProperties};
use smithay::wayland::shell::wlr_layer::{Anchor, ExclusiveZone};

/// Use a fixed output scale.
const SCALE: i32 = 1;

/// Wayland output, typically a screen.
pub struct Output {
    pub exclusive: ExclusiveSpace,
    pub orientation: Orientation,
    pub scale: f64,
    global: Option<Global<WlOutput>>,
    output: SmithayOutput,
    mode: Mode,
}

impl Drop for Output {
    fn drop(&mut self) {
        if let Some(global) = self.global.take() {
            global.destroy();
        }
    }
}

impl Output {
    pub fn new(
        display: &mut Display,
        name: impl Into<String>,
        mode: Mode,
        properties: PhysicalProperties,
    ) -> Self {
        let (output, global) = SmithayOutput::new(display, name.into(), properties, None);

        // Set output mode.
        output.change_current_state(Some(mode), None, Some(SCALE), None);
        output.set_preferred(mode);

        Self {
            orientation: Orientation::Portrait,
            global: Some(global),
            scale: SCALE as f64,
            output,
            mode,
            exclusive: Default::default(),
        }
    }

    /// Create a new dummy output.
    pub fn new_dummy(display: &mut Display) -> Self {
        let mode = Mode { size: (0, 0).into(), refresh: 0 };
        Output::new(display, "dummy-0", mode, PhysicalProperties {
            subpixel: Subpixel::Unknown,
            model: "dummy-0".into(),
            make: "dummy-0".into(),
            size: (0, 0).into(),
        })
    }

    /// Update the output's active mode.
    pub fn set_mode(&mut self, mode: Mode) {
        self.output.change_current_state(Some(mode), None, Some(SCALE), None);
        self.output.set_preferred(mode);
        self.mode = mode;
    }

    /// Primary window dimensions.
    pub fn primary_rectangle(&self, secondary_visible: bool) -> Rectangle<i32, Logical> {
        let available = self.available();
        let size: Size<i32, Logical> = match (self.orientation, secondary_visible) {
            (Orientation::Portrait, true) => (available.size.w, (available.size.h + 1) / 2).into(),
            (Orientation::Landscape, true) => ((available.size.w + 1) / 2, available.size.h).into(),
            (_, false) => (available.size.w, available.size.h).into(),
        };

        Rectangle::from_loc_and_size(available.loc, size)
    }

    /// Secondary window dimensions.
    pub fn secondary_rectangle(&self) -> Rectangle<i32, Logical> {
        let available = self.available();
        let mut loc = available.loc;
        match self.orientation {
            Orientation::Portrait => {
                let size: Size<i32, Logical> = (available.size.w, available.size.h / 2).into();
                loc.y += (available.size.h + 1) / 2;
                Rectangle::from_loc_and_size(loc, size)
            },
            Orientation::Landscape => {
                let size: Size<i32, Logical> = (available.size.w / 2, available.size.h).into();
                loc.x += (available.size.w + 1) / 2;
                Rectangle::from_loc_and_size(loc, size)
            },
        }
    }

    /// Output dimensions including reserved space.
    pub fn screen_size(&self) -> Size<i32, Logical> {
        self.mode.size.to_f64().to_logical(self.scale).to_i32_round()
    }

    /// Area of the output not reserved for layer shell windows.
    pub fn available(&self) -> Rectangle<i32, Logical> {
        let loc = (self.exclusive.left, self.exclusive.top);
        let mut size = self.screen_size();
        size.w -= self.exclusive.left + self.exclusive.right;
        size.h -= self.exclusive.top + self.exclusive.bottom;
        Rectangle::from_loc_and_size(loc, size)
    }

    /// Resize the output.
    pub fn resize(&mut self, size: Size<i32, Physical>) {
        self.mode.size = size;
    }

    /// Duration between frames in milliseconds.
    pub fn frame_interval(&self) -> u64 {
        1_000_000 / self.mode.refresh as u64
    }
}

impl Deref for Output {
    type Target = SmithayOutput;

    fn deref(&self) -> &Self::Target {
        &self.output
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

/// Device output orientation.
#[derive(Copy, Clone, Debug)]
pub enum Orientation {
    Landscape,
    Portrait,
}
