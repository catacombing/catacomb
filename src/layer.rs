//! Layer shell windows.

use std::cell::RefCell;
use std::rc::Rc;

use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::wayland::shell::wlr_layer::{Layer, LayerSurface};

use crate::output::Output;
use crate::window::{Surface, Window};

/// Layer shell windows.
#[derive(Debug, Default)]
pub struct Layers {
    background: Vec<Rc<RefCell<Window<LayerSurface>>>>,
    bottom: Vec<Rc<RefCell<Window<LayerSurface>>>>,
    top: Vec<Rc<RefCell<Window<LayerSurface>>>>,
    overlay: Vec<Rc<RefCell<Window<LayerSurface>>>>,
}

impl Layers {
    /// Add a new layer shell window.
    pub fn add(&mut self, layer: Layer, surface: LayerSurface) {
        let window = Rc::new(RefCell::new(Window::new(surface)));
        match layer {
            Layer::Background => self.background.push(window),
            Layer::Bottom => self.bottom.push(window),
            Layer::Top => self.top.push(window),
            Layer::Overlay => self.overlay.push(window),
        }
    }

    /// Request new frames for all layer windows.
    pub fn iter(&mut self) -> impl Iterator<Item = &Rc<RefCell<Window<LayerSurface>>>> {
        self.background.iter().chain(&self.bottom).chain(&self.top).chain(&self.overlay)
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
        for window in self.iter() {
            window.borrow_mut().request_frame(runtime);
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
    fn apply_transaction_filter<S: Surface>(window: &Rc<RefCell<Window<S>>>) -> bool {
        let mut window = window.borrow_mut();
        window.apply_transaction();
        window.alive()
    }
}
