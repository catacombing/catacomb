//! Layer shell windows.

use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point};
use smithay::wayland::shell::wlr_layer::Layer;

use crate::windows::surface::CatacombLayerSurface;
use crate::windows::window::Window;

type LayerWindow = Window<CatacombLayerSurface>;

/// Layer shell windows.
#[derive(Debug, Default)]
pub struct Layers {
    pub focus: Option<(WlSurface, Option<String>)>,

    background: Vec<LayerWindow>,
    bottom: Vec<LayerWindow>,
    top: Vec<LayerWindow>,
    overlay: Vec<LayerWindow>,
}

impl Layers {
    /// Add a new layer shell window.
    pub fn add(&mut self, window: LayerWindow) {
        match window.surface.layer() {
            Layer::Background => self.background.push(window),
            Layer::Bottom => self.bottom.push(window),
            Layer::Top => self.top.push(window),
            Layer::Overlay => self.overlay.push(window),
        }
    }

    /// Get number of layer shell clients.
    pub fn len(&self) -> usize {
        self.background.len() + self.bottom.len() + self.top.len() + self.overlay.len()
    }

    /// Iterate over all layer shell windows.
    pub fn iter(&self) -> impl Iterator<Item = &LayerWindow> {
        self.background.iter().chain(&self.bottom).chain(&self.top).chain(&self.overlay)
    }

    /// Iterate mutably over all layer shell windows, top to bottom.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LayerWindow> {
        self.background
            .iter_mut()
            .chain(&mut self.bottom)
            .chain(&mut self.top)
            .chain(&mut self.overlay)
            .rev()
    }

    /// Iterate over layer shell background windows, top to bottom.
    pub fn background(&self) -> impl Iterator<Item = &LayerWindow> {
        self.background.iter().chain(&self.bottom).rev()
    }

    /// Iterate over layer shell foreground windows, top to bottom.
    pub fn foreground(&self) -> impl Iterator<Item = &LayerWindow> {
        self.top.iter().chain(&self.overlay).rev()
    }

    /// Iterate over layer shell overlay windows, top to bottom.
    pub fn overlay(&self) -> impl Iterator<Item = &LayerWindow> {
        self.overlay.iter().rev()
    }

    /// Iterate over layer shell overlay windows, top to bottom.
    pub fn overlay_mut(&mut self) -> impl Iterator<Item = &mut LayerWindow> {
        self.overlay.iter_mut().rev()
    }

    /// Request new frames from all layer shell windows.
    pub fn request_frames(&mut self, runtime: u32) {
        for window in self.iter_mut() {
            window.request_frame(runtime);
        }
    }

    /// Foreground window at the specified position.
    pub fn foreground_window_at(
        &self,
        output_scale: f64,
        position: Point<f64, Logical>,
    ) -> Option<&LayerWindow> {
        self.overlay.iter().rev().find(|window| window.contains(output_scale, position)).or_else(
            || self.top.iter().rev().find(|window| window.contains(output_scale, position)),
        )
    }

    /// Background window at the specified position.
    pub fn background_window_at(
        &self,
        output_scale: f64,
        position: Point<f64, Logical>,
    ) -> Option<&LayerWindow> {
        self.bottom.iter().rev().find(|window| window.contains(output_scale, position)).or_else(
            || self.background.iter().rev().find(|window| window.contains(output_scale, position)),
        )
    }

    /// Overlay window at the specified position.
    pub fn overlay_window_at(
        &self,
        output_scale: f64,
        position: Point<f64, Logical>,
    ) -> Option<&LayerWindow> {
        self.overlay.iter().rev().find(|window| window.contains(output_scale, position))
    }

    /// Apply all pending transactional updates.
    pub fn apply_transaction(&mut self) {
        Self::apply_window_transactions(&mut self.background);
        Self::apply_window_transactions(&mut self.bottom);
        Self::apply_window_transactions(&mut self.top);
        Self::apply_window_transactions(&mut self.overlay);
    }

    /// Apply transactions to all windows and remove dead ones.
    fn apply_window_transactions(windows: &mut Vec<LayerWindow>) {
        for i in (0..windows.len()).rev() {
            if windows[i].alive() {
                windows[i].apply_transaction();
            } else {
                windows.remove(i);
            }
        }
    }
}
