//! Output region handling.

use std::ops::Deref;

use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;
use smithay::reexports::wayland_server::{Display, Global};
use smithay::utils::{Logical, Rectangle, Size};
use smithay::wayland::output::{Mode, Output as SmithayOutput, PhysicalProperties};

/// Use a fixed output scale.
const SCALE: i32 = 1;

/// Wayland output, typically a screen.
pub struct Output {
    pub orientation: Orientation,
    pub scale: f64,
    pub mode: Mode,
    global: Option<Global<WlOutput>>,
    output: SmithayOutput,
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
        }
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

impl Deref for Output {
    type Target = SmithayOutput;

    fn deref(&self) -> &Self::Target {
        &self.output
    }
}

/// Device output orientation.
#[derive(Copy, Clone, Debug)]
pub enum Orientation {
    Landscape,
    Portrait,
}
