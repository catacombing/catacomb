//! Output region handling.

use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;
use smithay::reexports::wayland_server::{Display, Global};
use smithay::utils::{Logical, Size};
use smithay::wayland::output::{Mode, Output as SmithayOutput, PhysicalProperties};

/// Use a fixed output scale.
const SCALE: i32 = 1;

/// Wayland output, typically a screen.
pub struct Output {
    pub scale: f64,

    global: Option<Global<WlOutput>>,
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

        Self { global: Some(global), scale: SCALE as f64, mode }
    }

    /// Output dimensions.
    pub fn size(&self) -> Size<i32, Logical> {
        self.mode.size.to_f64().to_logical(self.scale).to_i32_round()
    }
}
