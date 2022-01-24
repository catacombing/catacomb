//! Layer shell windows.

use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::wayland::shell::wlr_layer::Layer;

use crate::output::Output;
use crate::window::{CatacombLayerSurface, Window};

type LayerWindow = Rc<RefCell<Window<CatacombLayerSurface>>>;

/// Layer shell windows.
#[derive(Debug, Default)]
pub struct Layers {
    background: Vec<LayerWindow>,
    bottom: Vec<LayerWindow>,
    top: Vec<LayerWindow>,
    overlay: Vec<LayerWindow>,
}

impl Layers {
    /// Add a new layer shell window.
    pub fn add(&mut self, layer: Layer, surface: CatacombLayerSurface) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        match layer {
            Layer::Background => self.background.push(window),
            Layer::Bottom => self.bottom.push(window),
            Layer::Top => self.top.push(window),
            Layer::Overlay => self.overlay.push(window),
        }
    }

    /// Request new frames for all layer windows.
    pub fn iter(&self) -> impl Iterator<Item = Ref<'_, Window<CatacombLayerSurface>>> {
        self.background
            .iter()
            .chain(&self.bottom)
            .chain(&self.top)
            .chain(&self.overlay)
            .map(|window| window.borrow())
    }

    /// Request new frames for all layer windows.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = RefMut<'_, Window<CatacombLayerSurface>>> {
        self.background
            .iter()
            .chain(&self.bottom)
            .chain(&self.top)
            .chain(&self.overlay)
            .map(|window| window.borrow_mut())
    }

    /// Draw background/bottom layer windows.
    pub fn draw_background(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
    ) {
        for window in &self.background {
            window.borrow_mut().draw(renderer, frame, output, 1., None);
        }

        for window in &self.bottom {
            window.borrow_mut().draw(renderer, frame, output, 1., None);
        }
    }

    /// Draw top/overlay layer windows.
    pub fn draw_foreground(
        &mut self,
        renderer: &mut Gles2Renderer,
        frame: &mut Gles2Frame,
        output: &Output,
    ) {
        for window in &self.top {
            window.borrow_mut().draw(renderer, frame, output, 1., None);
        }

        for window in &self.overlay {
            window.borrow_mut().draw(renderer, frame, output, 1., None);
        }
    }

    /// Request new frames from all layer shell windows.
    pub fn request_frames(&mut self, runtime: u32) {
        for mut window in self.iter_mut() {
            window.request_frame(runtime);
        }
    }

    /// Apply all pending transactional updates.
    pub fn apply_transaction(&mut self) {
        self.background.retain(Self::apply_transaction_filter);
        self.bottom.retain(Self::apply_transaction_filter);
        self.top.retain(Self::apply_transaction_filter);
        self.overlay.retain(Self::apply_transaction_filter);
    }

    /// Apply the window's current transaction then check whether it still is alive.
    fn apply_transaction_filter(window: &LayerWindow) -> bool {
        let mut window = window.borrow_mut();
        window.apply_transaction();
        window.alive()
    }
}
