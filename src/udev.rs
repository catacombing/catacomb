//! Udev backend.

use std::error::Error;
use std::os::unix::io::FromRawFd;
use std::path::PathBuf;
use std::process;
use std::time::Duration;

use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::allocator::gbm::GbmDevice;
use smithay::backend::drm::{DrmDevice, DrmDeviceFd, DrmEvent, GbmBufferedSurface};
use smithay::backend::egl::context::EGLContext;
use smithay::backend::egl::display::EGLDisplay;
use smithay::backend::libinput::{LibinputInputBackend, LibinputSessionInterface};
use smithay::backend::renderer::gles2::{ffi, Gles2Renderer};
use smithay::backend::renderer::{
    self, Bind, BufferType, Frame, ImportAll, ImportDma, ImportEgl, Renderer,
};
use smithay::backend::session::libseat::LibSeatSession;
use smithay::backend::session::{Event as SessionEvent, Session};
use smithay::backend::udev;
use smithay::backend::udev::{UdevBackend, UdevEvent};
use smithay::output::{Mode, PhysicalProperties, Subpixel};
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::{Dispatcher, EventLoop, LoopHandle, RegistrationToken};
use smithay::reexports::drm::control::connector::State as ConnectorState;
use smithay::reexports::drm::control::property::{
    Handle as PropertyHandle, Value as PropertyValue,
};
use smithay::reexports::drm::control::Device;
use smithay::reexports::input::Libinput;
use smithay::reexports::nix::fcntl::OFlag;
use smithay::reexports::nix::sys::stat::dev_t as DeviceId;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_shm;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{DevPath, DeviceFd, Physical, Rectangle};
use smithay::wayland::shm;

use crate::catacomb::{Catacomb, Damage};
use crate::drawing::Graphics;
use crate::ipc_server;
use crate::output::Output;
use crate::windows::Windows;

pub fn run() {
    let mut event_loop = EventLoop::try_new().expect("event loop");
    let udev = Udev::new(event_loop.handle());
    let mut catacomb = Catacomb::new(event_loop.handle(), udev);

    // Create backend and add presently connected devices.
    let backend = UdevBackend::new(&catacomb.seat_name, None).expect("init udev");
    for (_, path) in backend.device_list() {
        add_device(&mut catacomb, path.into());
    }

    // Setup hardware acceleration.
    let output_device = catacomb.backend.output_device.as_ref();
    let formats = output_device.map(|device| device.renderer.dmabuf_formats().copied().collect());
    catacomb.dmabuf_state.create_global::<Catacomb, _>(
        &catacomb.display_handle,
        formats.unwrap_or_default(),
        None,
    );

    // Setup input handling.

    let session = catacomb.backend.session.clone();
    let mut context = Libinput::new_with_udev::<LibinputSessionInterface<_>>(session.into());
    context.udev_assign_seat(&catacomb.seat_name).expect("assign seat");

    let input_backend = LibinputInputBackend::new(context, None);
    event_loop
        .handle()
        .insert_source(input_backend, |event, _, catacomb| catacomb.handle_input(event))
        .expect("insert input source");

    // Handle device events.
    event_loop
        .handle()
        .insert_source(backend, move |event, _, catacomb| match event {
            UdevEvent::Added { path, .. } => add_device(catacomb, path),
            UdevEvent::Changed { device_id } => {
                let _ = catacomb.backend.change_device(
                    &catacomb.display_handle,
                    &mut catacomb.windows,
                    device_id,
                );
            },
            UdevEvent::Removed { device_id } => catacomb.backend.remove_device(device_id),
        })
        .expect("insert udev source");

    // Start IPC socket listener.
    ipc_server::spawn_ipc_socket(&event_loop.handle(), &catacomb.socket_name)
        .expect("spawn IPC socket");

    // Continously dispatch event loop.
    while !catacomb.terminated {
        if let Err(error) = event_loop.dispatch(None, &mut catacomb) {
            eprintln!("Event loop error: {error}");
            break;
        }
        catacomb.display.borrow_mut().flush_clients().expect("flushing clients");
    }
}

/// Add udev device, automatically kicking off rendering for it.
fn add_device(catacomb: &mut Catacomb, path: PathBuf) {
    let _ = catacomb.backend.add_device(&catacomb.display_handle, &mut catacomb.windows, path);

    // Kick-off rendering.
    catacomb.create_frame();
}

/// Udev backend shared state.
pub struct Udev {
    scheduled_redraws: Vec<RegistrationToken>,
    event_loop: LoopHandle<'static, Catacomb>,
    output_device: Option<OutputDevice>,
    session: LibSeatSession,
    gpu: Option<PathBuf>,
}

impl Udev {
    fn new(event_loop: LoopHandle<'static, Catacomb>) -> Self {
        // Initialize the VT session.
        let (session, notifier) = match LibSeatSession::new(None) {
            Ok(session) => session,
            Err(_) => {
                eprintln!(
                    "[error] Unable to start libseat session: Ensure logind/seatd service is \
                     running with no active session"
                );
                process::exit(666);
            },
        };

        // Register notifier for handling session events.
        event_loop
            .insert_source(notifier, |event, _, catacomb| match event {
                SessionEvent::ActivateSession => {
                    catacomb.event_loop.insert_idle(Catacomb::create_frame);
                },
                SessionEvent::PauseSession => (),
            })
            .expect("insert notifier source");

        // Find active GPUs for hardware acceleration.
        let gpu = udev::primary_gpu(session.seat()).ok().flatten();

        Self {
            session,
            event_loop,
            gpu,
            scheduled_redraws: Default::default(),
            output_device: Default::default(),
        }
    }

    /// Get Wayland seat name.
    pub fn seat_name(&self) -> String {
        self.session.seat()
    }

    /// Change Unix TTY.
    pub fn change_vt(&mut self, vt: i32) {
        let _ = self.session.change_vt(vt);
    }

    /// Set power saving state.
    pub fn set_sleep(&mut self, sleep: bool) {
        let output_device = match &mut self.output_device {
            Some(output_device) => output_device,
            None => return,
        };

        output_device.set_enabled(!sleep);

        // Request immediate redraw, so vblanks start coming in again.
        if !sleep {
            self.schedule_redraw(Duration::ZERO);
        }
    }

    /// Render a frame.
    pub fn render(&mut self, windows: &mut Windows, damage: &mut Damage) {
        if let Some(output_device) = &mut self.output_device {
            let _ = output_device.render(windows, damage);
        }
    }

    /// Get the current output's renderer.
    pub fn renderer(&mut self) -> Option<&mut Gles2Renderer> {
        self.output_device.as_mut().map(|output_device| &mut output_device.renderer)
    }

    /// Copy framebuffer region into another buffer.
    pub fn copy_framebuffer(
        &mut self,
        buffer: &WlBuffer,
        rect: Rectangle<i32, Physical>,
    ) -> Result<(), Box<dyn Error>> {
        if let Some(output_device) = &mut self.output_device {
            output_device.copy_framebuffer(buffer, rect)?;
        }
        Ok(())
    }

    /// Request a redraw once `duration` has passed.
    pub fn schedule_redraw(&mut self, duration: Duration) {
        let token = self
            .event_loop
            .insert_source(Timer::from_duration(duration), move |_, _, catacomb| {
                catacomb.create_frame();
                TimeoutAction::Drop
            })
            .expect("insert render timer");
        self.scheduled_redraws.push(token);
    }

    /// Cancel all pending redraws.
    pub fn cancel_scheduled_redraws(&mut self) {
        for scheduled_redraw in self.scheduled_redraws.drain(..) {
            self.event_loop.remove(scheduled_redraw);
        }
    }

    fn add_device(
        &mut self,
        display_handle: &DisplayHandle,
        windows: &mut Windows,
        path: PathBuf,
    ) -> Result<(), Box<dyn Error>> {
        let open_flags = OFlag::O_RDWR | OFlag::O_CLOEXEC | OFlag::O_NOCTTY | OFlag::O_NONBLOCK;
        let fd = self.session.open(&path, open_flags)?;
        let device_fd = unsafe { DrmDeviceFd::new(DeviceFd::from_raw_fd(fd), None) };

        let drm = DrmDevice::new(device_fd.clone(), true, None)?;
        let gbm = GbmDevice::new(device_fd)?;

        let display = EGLDisplay::new(gbm.clone(), None)?;
        let context = EGLContext::new(&display, None)?;

        let mut renderer = unsafe { Gles2Renderer::new(context, None).expect("create renderer") };

        // Initialize GPU for EGL rendering.
        if Some(path) == self.gpu {
            let _ = renderer.bind_wl_display(display_handle);
        }

        // Create the surface we will render to.
        let gbm_surface = self
            .create_gbm_surface(display_handle, windows, &renderer, &drm, &gbm)
            .ok_or("gbm surface")?;

        // Listen for VBlanks.
        let device_id = drm.device_id();
        let dispatcher = Dispatcher::new(drm, move |event, _, catacomb: &mut Catacomb| {
            match event {
                DrmEvent::VBlank(_crtc) => catacomb.create_frame(),
                DrmEvent::Error(error) => eprintln!("DRM error: {error}"),
            };
        });
        let token = self.event_loop.register_dispatcher(dispatcher.clone())?;

        // Create OpenGL textures.
        let graphics = Graphics::new(&mut renderer);

        self.output_device = Some(OutputDevice {
            gbm_surface,
            graphics,
            renderer,
            token,
            gbm,
            drm: dispatcher,
            id: device_id,
        });

        Ok(())
    }

    fn remove_device(&mut self, device_id: DeviceId) {
        let output_device = self.output_device.take();
        if let Some(mut output_device) = output_device.filter(|device| device.id == device_id) {
            self.event_loop.remove(output_device.token);

            // Disable hardware acceleration when the GPU is removed.
            if output_device.gbm.dev_path() == self.gpu {
                output_device.renderer.unbind_wl_display();
            }
        }
    }

    fn change_device(
        &mut self,
        display_handle: &DisplayHandle,
        windows: &mut Windows,
        device_id: DeviceId,
    ) -> Result<(), Box<dyn Error>> {
        let device = self.output_device.as_ref().filter(|dev| dev.id == device_id);
        let path = device.and_then(|device| device.gbm.dev_path());
        if let Some(path) = path {
            self.remove_device(device_id);
            self.add_device(display_handle, windows, path)?;
        }

        Ok(())
    }

    /// Create a new GBM surface for a device.
    fn create_gbm_surface(
        &mut self,
        display: &DisplayHandle,
        windows: &mut Windows,
        renderer: &Gles2Renderer,
        drm: &DrmDevice,
        gbm: &GbmDevice<DrmDeviceFd>,
    ) -> Option<GbmBufferedSurface<GbmDevice<DrmDeviceFd>, ()>> {
        let formats = Bind::<Dmabuf>::supported_formats(renderer)?;
        let resources = drm.resource_handles().ok()?;

        // Find the first connected output port.
        let connector = resources.connectors().iter().find_map(|conn| {
            drm.get_connector(*conn, true)
                .ok()
                .filter(|conn| conn.state() == ConnectorState::Connected)
        })?;
        let connector_mode = *connector.modes().get(0)?;

        let surface = connector
            // Get all available encoders.
            .encoders()
            .iter()
            .flat_map(|handle| drm.get_encoder(*handle))
            // Get all CRTCs compatible with the encoder.
            .flat_map(|encoder| resources.filter_crtcs(encoder.possible_crtcs()))
            // Try to create a DRM surface.
            .flat_map(|crtc| drm.create_surface(crtc, connector_mode, &[connector.handle()]))
            // Yield the first successful GBM buffer creation.
            .find_map(|surface| {
                GbmBufferedSurface::new(surface, gbm.clone(), formats.clone(), None).ok()
            })?;

        let (width, height) = connector_mode.size();
        let mode = Mode {
            size: (width as i32, height as i32).into(),
            refresh: connector_mode.vrefresh() as i32 * 1000,
        };

        let (physical_width, physical_height) = connector.size().unwrap_or((0, 0));
        let output_name = format!("{:?}", connector.interface());

        windows.set_output(Output::new(display, output_name, mode, PhysicalProperties {
            size: (physical_width as i32, physical_height as i32).into(),
            subpixel: Subpixel::Unknown,
            model: "Generic DRM".into(),
            make: "Catacomb".into(),
        }));

        Some(surface)
    }
}

/// Target device for rendering.
pub struct OutputDevice {
    gbm_surface: GbmBufferedSurface<GbmDevice<DrmDeviceFd>, ()>,
    drm: Dispatcher<'static, DrmDevice, Catacomb>,
    renderer: Gles2Renderer,
    gbm: GbmDevice<DrmDeviceFd>,
    graphics: Graphics,
    id: DeviceId,

    token: RegistrationToken,
}

impl OutputDevice {
    /// Get DRM property handle.
    pub fn get_drm_property(&self, name: &str) -> Option<PropertyHandle> {
        let crtc = self.gbm_surface.crtc();
        let drm = self.drm.as_source_ref();

        // Get all available properties.
        let properties = drm.get_properties(crtc).ok()?;
        let (property_handles, _) = properties.as_props_and_values();

        // Find property matching the requested name.
        property_handles.iter().find_map(|handle| {
            let property_info = drm.get_property(*handle).ok()?;
            let property_name = property_info.name().to_str().ok()?;

            (property_name == name).then_some(*handle)
        })
    }

    /// Set output DPMS state.
    fn set_enabled(&mut self, enabled: bool) {
        let property = match self.get_drm_property("ACTIVE") {
            Some(property) => property,
            None => return,
        };

        let crtc = self.gbm_surface.crtc();
        let drm = self.drm.as_source_ref();

        let value = PropertyValue::Boolean(enabled);
        let _ = drm.set_property(crtc, property, value.into());
    }

    /// Render a frame.
    fn render(&mut self, windows: &mut Windows, damage: &mut Damage) -> Result<(), Box<dyn Error>> {
        // Mark the current frame as submitted.
        self.gbm_surface.frame_submitted()?;

        // Bind the next buffer to render into.
        let (dmabuf, age) = self.gbm_surface.next_buffer()?;
        self.renderer.bind(dmabuf)?;

        // Draw the current frame into the buffer.
        let transform = windows.orientation().output_transform();
        let output_size = windows.output().physical_resolution();
        let mut frame = self.renderer.render(output_size, transform)?;
        windows.draw(&mut frame, &self.graphics, damage, age);

        // XXX: This must be done before `queue_buffer` to prevent rendering artifacts.
        let _ = frame.finish();

        // Queue buffer for rendering.
        self.gbm_surface.queue_buffer(())?;

        Ok(())
    }

    /// Copy framebuffer region into a Wayland buffer.
    fn copy_framebuffer(
        &mut self,
        buffer: &WlBuffer,
        rect: Rectangle<i32, Physical>,
    ) -> Result<(), Box<dyn Error>> {
        match renderer::buffer_type(buffer) {
            Some(BufferType::Shm) => self.copy_framebuffer_shm(buffer, rect),
            Some(BufferType::Dma) => self.copy_framebuffer_dma(buffer, rect),
            Some(format) => Err(format!("unsupported buffer format: {format:?}").into()),
            None => Err("invalid target buffer".into()),
        }
    }

    /// Copy framebuffer region into a SHM buffer.
    #[allow(clippy::identity_op)]
    fn copy_framebuffer_shm(
        &mut self,
        buffer: &WlBuffer,
        rect: Rectangle<i32, Physical>,
    ) -> Result<(), Box<dyn Error>> {
        // Manually copy array to SHM buffer to account for stride.
        shm::with_buffer_contents_mut(buffer, |shm_buffer, buffer_data| {
            // Ensure buffer is in an acceptable format.
            if buffer_data.format != wl_shm::Format::Argb8888
                || buffer_data.stride != rect.size.w * 4
                || buffer_data.height != rect.size.h
                || shm_buffer.len() as i32 != buffer_data.stride * buffer_data.height
            {
                return Err::<(), Box<dyn Error>>("Invalid buffer format".into());
            }

            // Copy framebuffer data to the SHM buffer.
            self.renderer.with_context(|gl| unsafe {
                gl.ReadPixels(
                    rect.loc.x,
                    rect.loc.y,
                    rect.size.w,
                    rect.size.h,
                    ffi::RGBA,
                    ffi::UNSIGNED_BYTE,
                    shm_buffer.as_mut_ptr().cast(),
                );
            })?;

            // Convert OpenGL's RGBA to ARGB.
            for i in 0..(rect.size.w * rect.size.h) as usize {
                shm_buffer.swap(i * 4, i * 4 + 2);
            }

            Ok(())
        })?
    }

    /// Copy framebuffer region into a DMA buffer.
    fn copy_framebuffer_dma(
        &mut self,
        buffer: &WlBuffer,
        rect: Rectangle<i32, Physical>,
    ) -> Result<(), Box<dyn Error>> {
        let buffer_size = renderer::buffer_dimensions(buffer).ok_or("unexpected buffer type")?;
        let damage = [Rectangle::from_loc_and_size((0, 0), buffer_size)];
        let texture = self
            .renderer
            .import_buffer(buffer, None, &damage)
            .ok_or("unexpected buffer type")??;

        self.renderer.with_context(|gl| unsafe {
            gl.BindTexture(ffi::TEXTURE_2D, texture.tex_id());
            gl.CopyTexSubImage2D(
                ffi::TEXTURE_2D,
                0,
                0,
                0,
                rect.loc.x,
                rect.loc.y,
                rect.size.w,
                rect.size.h,
            );
        })?;

        Ok(())
    }
}
