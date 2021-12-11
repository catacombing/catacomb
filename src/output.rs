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
        let output_size = self.size();
        let size: Size<i32, Logical> = match (self.orientation, secondary_visible) {
            (Orientation::Portrait, true) => (output_size.w, (output_size.h + 1) / 2).into(),
            (Orientation::Landscape, true) => ((output_size.w + 1) / 2, output_size.h).into(),
            (_, false) => (output_size.w, output_size.h).into(),
        };
        Rectangle::from_loc_and_size((0, 0), size)
    }

    /// Secondary window dimensions.
    pub fn secondary_rectangle(&self) -> Rectangle<i32, Logical> {
        let output_size = self.size();
        match self.orientation {
            Orientation::Portrait => {
                let size: Size<i32, Logical> = (output_size.w, output_size.h / 2).into();
                Rectangle::from_loc_and_size((0, (output_size.h + 1) / 2), size)
            },
            Orientation::Landscape => {
                let size: Size<i32, Logical> = (output_size.w / 2, output_size.h).into();
                Rectangle::from_loc_and_size(((output_size.w + 1) / 2, 0), size)
            },
        }
    }

    /// Output dimensions.
    pub fn size(&self) -> Size<i32, Logical> {
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
