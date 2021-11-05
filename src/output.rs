//! Output region handling.

use smithay::utils::{Logical, Rectangle, Size};
use smithay::wayland::output::Mode;

/// Use a fixed output scale.
const SCALE: i32 = 1;

/// Wayland output, typically a screen.
pub struct Output {
    pub orientation: Orientation,
    pub scale: f64,
    pub mode: Mode,
}

impl Output {
    pub fn new(mode: Mode) -> Self {
        Self { orientation: Orientation::Portrait, scale: SCALE as f64, mode }
    }

    /// Primary window dimensions.
    pub fn primary_rectangle(&self, secondary_visible: bool) -> Rectangle<i32, Logical> {
        let size = self.size();
        match (self.orientation, secondary_visible) {
            (Orientation::Portrait, true) => {
                Rectangle::from_loc_and_size((0, 0), (size.w, (size.h + 1) / 2))
            },
            (Orientation::Landscape, true) => {
                Rectangle::from_loc_and_size((0, 0), ((size.w + 1) / 2, size.h))
            },
            (_, false) => Rectangle::from_loc_and_size((0, 0), (size.w, size.h)),
        }
    }

    /// Secondary window dimensions.
    pub fn secondary_rectangle(&self) -> Rectangle<i32, Logical> {
        let size = self.size();
        match self.orientation {
            Orientation::Portrait => {
                Rectangle::from_loc_and_size((0, (size.h + 1) / 2), (size.w, size.h / 2))
            },
            Orientation::Landscape => {
                Rectangle::from_loc_and_size(((size.w + 1) / 2, 0), (size.w / 2, size.h))
            },
        }
    }

    /// Output dimensions.
    fn size(&self) -> Size<i32, Logical> {
        self.mode.size.to_f64().to_logical(self.scale).to_i32_round()
    }
}

/// Device output orientation.
#[derive(Copy, Clone, Debug)]
pub enum Orientation {
    Landscape,
    Portrait,
}
