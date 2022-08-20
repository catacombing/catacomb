use std::cell::RefCell;
use std::error::Error;
use std::rc::Rc;
use std::time::Duration;

use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::renderer::gles2::{Gles2Frame, Gles2Renderer};
use smithay::backend::renderer::{ImportDma, ImportEgl, Renderer, TextureFilter};
use smithay::backend::winit::{self, WinitGraphicsBackend};
use smithay::delegate_dmabuf;
use smithay::reexports::calloop::EventLoop;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::wayland::dmabuf::{DmabufGlobal, DmabufHandler, DmabufState, ImportError};
use smithay::wayland::output::Mode;

use crate::catacomb::{Backend, Catacomb, Render};

struct Winit {
    graphics: Rc<RefCell<WinitGraphicsBackend>>,
    dmabuf_state: DmabufState,
}

impl Winit {
    fn new(graphics: &Rc<RefCell<WinitGraphicsBackend>>) -> Self {
        Self { dmabuf_state: DmabufState::new(), graphics: graphics.clone() }
    }
}

impl Backend for Winit {
    fn seat_name(&self) -> String {
        String::from("seat-0")
    }
}

pub fn run() {
    let (graphics, mut input) = winit::init(None).expect("init winit");
    let graphics = Rc::new(RefCell::new(graphics));
    graphics.borrow_mut().bind().expect("binding renderer");
    let _ = graphics.borrow_mut().renderer().downscale_filter(TextureFilter::Linear);

    let winit = Winit::new(&graphics);

    let mut event_loop = EventLoop::try_new().expect("event loop");
    let mut catacomb = Catacomb::new(event_loop.handle(), winit);

    // Set the output size.
    let mode = Mode { size: graphics.borrow().window_size().physical_size, refresh: 200_000 };
    catacomb.output.set_mode(mode);

    // Setup hardware acceleration.
    if graphics.borrow_mut().renderer().bind_wl_display(&catacomb.display_handle).is_ok() {
        let formats: Vec<_> = graphics.borrow_mut().renderer().dmabuf_formats().cloned().collect();
        catacomb.backend.dmabuf_state.create_global::<Catacomb<Winit>, _>(
            &catacomb.display_handle,
            formats,
            None,
        );
    }

    loop {
        if input.dispatch_new_events(|event| catacomb.handle_winit_input(event)).is_err() {
            eprintln!("input error");
            break;
        }

        catacomb.create_frame(&mut *graphics.borrow_mut());

        // NOTE: The timeout picked here is 5ms to allow for up to 200 FPS. Increasing
        // it would reduce the framerate, while decreasing it would mean that most of
        // the vblank interval is spent not doing anything, rather than handling events.
        if event_loop.dispatch(Some(Duration::from_millis(5)), &mut catacomb).is_err() {
            eprintln!("event loop error");
            break;
        }

        catacomb.display.borrow_mut().flush_clients().expect("flushing clients");
    }
}

impl Render for &mut WinitGraphicsBackend {
    fn render<B, F>(
        &mut self,
        catacomb: &mut Catacomb<B>,
        draw_fun: F,
    ) -> Result<(), Box<dyn Error>>
    where
        F: FnOnce(&mut Catacomb<B>, &mut Gles2Renderer, &mut Gles2Frame, u8),
    {
        let transform = catacomb.windows.orientation().transform();
        let logical_size = catacomb.output.resolution();
        let output_size = logical_size.to_physical(catacomb.output.scale());
        let buffer_age = self.buffer_age().unwrap_or(0) as u8;
        self.renderer()
            .render(output_size, transform, |renderer, frame| {
                draw_fun(catacomb, renderer, frame, buffer_age);
            })
            .expect("render");
        self.submit(None).expect("submit");
        Ok(())
    }
}

impl DmabufHandler for Catacomb<Winit> {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        &mut self.backend.dmabuf_state
    }

    fn dmabuf_imported(
        &mut self,
        _display: &DisplayHandle,
        _global: &DmabufGlobal,
        buffer: Dmabuf,
    ) -> Result<(), ImportError> {
        self.backend
            .graphics
            .borrow_mut()
            .renderer()
            .import_dmabuf(&buffer, None)
            .map_err(|_| ImportError::Failed)?;
        Ok(())
    }
}
delegate_dmabuf!(Catacomb<Winit>);
