//! Udev backend.

use std::collections::HashMap;
use std::error::Error;
use std::os::unix::io::FromRawFd;
use std::path::PathBuf;
use std::time::Duration;
use std::{mem, process, ptr};

use _linux_dmabuf::zv1::server::zwp_linux_dmabuf_feedback_v1::TrancheFlags;
use smithay::backend::allocator::dmabuf::Dmabuf;
use smithay::backend::allocator::gbm::{GbmAllocator, GbmBuffer, GbmBufferFlags, GbmDevice};
use smithay::backend::drm::compositor::{DrmCompositor as SmithayDrmCompositor, RenderFrameResult};
use smithay::backend::drm::{DrmDevice, DrmDeviceFd, DrmEvent};
use smithay::backend::egl::context::EGLContext;
use smithay::backend::egl::display::EGLDisplay;
use smithay::backend::libinput::{LibinputInputBackend, LibinputSessionInterface};
use smithay::backend::renderer::element::RenderElementStates;
use smithay::backend::renderer::gles2::{ffi, Gles2Renderbuffer, Gles2Renderer};
use smithay::backend::renderer::{
    self, utils, Bind, BufferType, Frame, ImportEgl, Offscreen, Renderer,
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
use smithay::reexports::wayland_protocols::wp::linux_dmabuf as _linux_dmabuf;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_shm;
use smithay::reexports::wayland_server::DisplayHandle;
use smithay::utils::{DevPath, DeviceFd, Physical, Rectangle, Size, Transform};
use smithay::wayland::dmabuf::{DmabufFeedback, DmabufFeedbackBuilder};
use smithay::wayland::{dmabuf, shm};

use crate::catacomb::Catacomb;
use crate::drawing::{CatacombElement, Graphics};
use crate::ipc_server;
use crate::output::Output;
use crate::protocols::screencopy::frame::Screencopy;
use crate::windows::Windows;

/// Default background color.
const CLEAR_COLOR: [f32; 4] = [1., 0., 1., 1.];

pub fn run() {
    let mut event_loop = EventLoop::try_new().expect("event loop");
    let udev = Udev::new(event_loop.handle());
    let mut catacomb = Catacomb::new(event_loop.handle(), udev);

    // Create backend and add presently connected devices.
    let backend = UdevBackend::new(&catacomb.seat_name).expect("init udev");
    for (_, path) in backend.device_list() {
        add_device(&mut catacomb, path.into());
    }

    // Setup hardware acceleration.
    let dmabuf_feedback = catacomb.backend.default_dmabuf_feedback().expect("dmabuf feedback");
    catacomb.dmabuf_state.create_global_with_default_feedback::<Catacomb>(
        &catacomb.display_handle,
        &dmabuf_feedback,
    );

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
    // Try to create the device.
    let result = catacomb.backend.add_device(&catacomb.display_handle, &mut catacomb.windows, path);

    // Kick-off rendering if the device creation was successful.
    if result.is_ok() {
        catacomb.create_frame();
    }
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
        let (session, notifier) = match LibSeatSession::new() {
            Ok(session) => session,
            Err(_) => {
                eprintln!(
                    "[error] Unable to start libseat session: Ensure logind/seatd service is \
                     running with no active session"
                );
                process::exit(666);
            },
        };

        // Setup input handling.

        let mut context =
            Libinput::new_with_udev::<LibinputSessionInterface<_>>(session.clone().into());
        context.udev_assign_seat(&session.seat()).expect("assign seat");

        let input_backend = LibinputInputBackend::new(context.clone());
        event_loop
            .insert_source(input_backend, |event, _, catacomb| catacomb.handle_input(event))
            .expect("insert input source");

        // Register notifier for handling session events.
        event_loop
            .insert_source(notifier, move |event, _, catacomb| match event {
                SessionEvent::ActivateSession => {
                    if let Err(err) = context.resume() {
                        eprintln!("Failed to resume libinput: {err:?}");
                    }

                    catacomb.create_frame();
                },
                SessionEvent::PauseSession => (),
            })
            .expect("insert notifier source");

        // Find active GPUs for hardware acceleration.
        let gpu = udev::primary_gpu(session.seat()).ok().flatten();

        Self {
            event_loop,
            session,
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
    ///
    /// Will return `true` if something was rendered.
    pub fn render(&mut self, windows: &mut Windows) -> bool {
        let output_device = match &mut self.output_device {
            Some(output_device) => output_device,
            None => return false,
        };

        output_device.render(windows).unwrap_or(false)
    }

    /// Get the current output's renderer.
    pub fn renderer(&mut self) -> Option<&mut Gles2Renderer> {
        self.output_device.as_mut().map(|output_device| &mut output_device.gles2)
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

    /// Stage a screencopy request for the next frame.
    pub fn request_screencopy(&mut self, screencopy: Screencopy) {
        let output_device = match &mut self.output_device {
            Some(output_device) => output_device,
            None => return,
        };

        // Stage new screencopy.
        output_device.screencopy = Some(screencopy);
    }

    /// Default dma surface feedback.
    fn default_dmabuf_feedback(&self) -> Result<DmabufFeedback, Box<dyn Error>> {
        match &self.output_device {
            Some(output_device) => output_device.default_dmabuf_feedback(),
            None => Err("missing output device".into()),
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
        let device_fd = unsafe { DrmDeviceFd::new(DeviceFd::from_raw_fd(fd)) };

        let (drm, drm_notifier) = DrmDevice::new(device_fd.clone(), true)?;
        let gbm = GbmDevice::new(device_fd)?;

        let display = EGLDisplay::new(gbm.clone())?;
        let context = EGLContext::new(&display)?;

        let mut gles2 = unsafe { Gles2Renderer::new(context).expect("create renderer") };

        // Initialize GPU for EGL rendering.
        if Some(path) == self.gpu {
            let _ = gles2.bind_wl_display(display_handle);
        }

        // Create the DRM compositor.
        let drm_compositor = self
            .create_drm_compositor(display_handle, windows, &gles2, &drm, &gbm)
            .ok_or("drm compositor")?;

        // Listen for VBlanks.
        let device_id = drm.device_id();
        let dispatcher =
            Dispatcher::new(drm_notifier, move |event, metadata, catacomb: &mut Catacomb| {
                match event {
                    DrmEvent::VBlank(_crtc) => {
                        let output_device = match &mut catacomb.backend.output_device {
                            Some(output_device) => output_device,
                            None => return,
                        };

                        // Mark the last frame as submitted.
                        let _ = output_device.drm_compositor.frame_submitted();

                        // Send presentation time feedback.
                        catacomb
                            .windows
                            .mark_presented(&output_device.last_render_states, metadata);

                        // Draw the next frame.
                        catacomb.create_frame();
                    },
                    DrmEvent::Error(error) => eprintln!("DRM error: {error}"),
                };
            });
        let token = self.event_loop.register_dispatcher(dispatcher.clone())?;

        // Create OpenGL textures.
        let graphics = Graphics::new(&mut gles2);

        // Initialize last render state as empty.
        let last_render_states = RenderElementStates { states: HashMap::new() };

        self.output_device = Some(OutputDevice {
            last_render_states,
            drm_compositor,
            graphics,
            gles2,
            token,
            gbm,
            drm,
            id: device_id,
            screencopy: Default::default(),
        });

        Ok(())
    }

    fn remove_device(&mut self, device_id: DeviceId) {
        let output_device = self.output_device.take();
        if let Some(mut output_device) = output_device.filter(|device| device.id == device_id) {
            self.event_loop.remove(output_device.token);

            // Disable hardware acceleration when the GPU is removed.
            if output_device.gbm.dev_path() == self.gpu {
                output_device.gles2.unbind_wl_display();
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

    /// Create the DRM compositor.
    fn create_drm_compositor(
        &mut self,
        display: &DisplayHandle,
        windows: &mut Windows,
        gles2: &Gles2Renderer,
        drm: &DrmDevice,
        gbm: &GbmDevice<DrmDeviceFd>,
    ) -> Option<DrmCompositor> {
        let formats = Bind::<Dmabuf>::supported_formats(gles2)?;
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
            // Find the ideal CRTC.
            .flat_map(|encoder| {
                // Get all CRTCs compatible with the encoder.
                let mut crtcs = resources.filter_crtcs(encoder.possible_crtcs());

                // Sort by maximum number of overlay planes.
                crtcs.sort_by_cached_key(|crtc| {
                    drm.planes(crtc).map_or(0, |planes| -(planes.overlay.len() as isize))
                });

                crtcs
            })
            // Try to create a DRM surface.
            .find_map(|crtc| drm.create_surface(crtc, connector_mode, &[connector.handle()]).ok())?;

        // Create GBM allocator.
        let gbm_flags = GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT;
        let allocator = GbmAllocator::new(gbm.clone(), gbm_flags);

        let (width, height) = connector_mode.size();
        let mode = Mode {
            size: (width as i32, height as i32).into(),
            refresh: connector_mode.vrefresh() as i32 * 1000,
        };

        // Update the output mode.

        let (physical_width, physical_height) = connector.size().unwrap_or((0, 0));
        let output_name = format!("{:?}", connector.interface());

        windows.set_output(Output::new(display, output_name, mode, PhysicalProperties {
            size: (physical_width as i32, physical_height as i32).into(),
            subpixel: Subpixel::Unknown,
            model: "Generic DRM".into(),
            make: "Catacomb".into(),
        }));

        // Create the compositor.
        DrmCompositor::new(
            windows.output().smithay_output(),
            surface,
            None,
            allocator,
            gbm.clone(),
            formats,
            Size::default(),
            None,
        )
        .ok()
    }
}

/// Target device for rendering.
pub struct OutputDevice {
    last_render_states: RenderElementStates,
    screencopy: Option<Screencopy>,
    drm_compositor: DrmCompositor,
    gbm: GbmDevice<DrmDeviceFd>,
    gles2: Gles2Renderer,
    graphics: Graphics,
    drm: DrmDevice,
    id: DeviceId,

    token: RegistrationToken,
}

impl OutputDevice {
    /// Get DRM property handle.
    fn get_drm_property(&self, name: &str) -> Option<PropertyHandle> {
        let crtc = self.drm_compositor.crtc();

        // Get all available properties.
        let properties = self.drm.get_properties(crtc).ok()?;
        let (property_handles, _) = properties.as_props_and_values();

        // Find property matching the requested name.
        property_handles.iter().find_map(|handle| {
            let property_info = self.drm.get_property(*handle).ok()?;
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

        let crtc = self.drm_compositor.crtc();

        let value = PropertyValue::Boolean(enabled);
        let _ = self.drm.set_property(crtc, property, value.into());
    }

    /// Render a frame.
    ///
    /// Will return `true` if something was rendered.
    fn render(&mut self, windows: &mut Windows) -> Result<bool, Box<dyn Error>> {
        let textures = windows.textures(&mut self.gles2, &mut self.graphics);
        let mut frame_result = self.drm_compositor.render_frame::<_, _, Gles2Renderbuffer>(
            &mut self.gles2,
            textures,
            CLEAR_COLOR,
        )?;
        let rendered = frame_result.damage.is_some();

        // TODO: Replace with `mem::take` once smithay/smithay#920 is merged.
        //
        // Update last render states.
        self.last_render_states =
            RenderElementStates { states: mem::take(&mut frame_result.states.states) };

        // Copy framebuffer for screencopy.
        if let Some(mut screencopy) = self.screencopy.take() {
            // Mark entire buffer as damaged.
            let region = screencopy.region();
            let damage = [Rectangle::from_loc_and_size((0, 0), region.size)];
            screencopy.damage(&damage);

            let buffer = screencopy.buffer();
            if let Ok(dmabuf) = dmabuf::get_dmabuf(buffer) {
                Self::copy_framebuffer_dma(&mut self.gles2, &frame_result, region, dmabuf)?;
            } else {
                // Ignore unknown buffer types.
                let buffer_type = renderer::buffer_type(buffer);
                if !matches!(buffer_type, Some(BufferType::Shm)) {
                    return Err(format!("unsupported buffer format: {buffer_type:?}").into());
                }

                self.copy_framebuffer_shm(windows, region, buffer)?;
            }

            // Mark screencopy frame as successful.
            screencopy.submit();
        }

        // Skip frame submission if everything used direct scanout.
        if rendered {
            self.drm_compositor.queue_frame(())?;
        }

        Ok(rendered)
    }

    /// Copy a region of the framebuffer to a DMA buffer.
    fn copy_framebuffer_dma(
        gles2: &mut Gles2Renderer,
        frame_result: &RenderFrameResult<GbmBuffer<()>, CatacombElement>,
        region: Rectangle<i32, Physical>,
        buffer: Dmabuf,
    ) -> Result<(), Box<dyn Error>> {
        // Bind the screencopy buffer as render target.
        gles2.bind(buffer)?;

        // Blit the framebuffer into the target buffer.
        let damage = [Rectangle::from_loc_and_size((0, 0), region.size)];
        frame_result.blit_frame_result(region.size, Transform::Normal, 1., gles2, damage, [])?;

        Ok(())
    }

    /// Copy a region of the framebuffer to an SHM buffer.
    fn copy_framebuffer_shm(
        &mut self,
        windows: &mut Windows,
        region: Rectangle<i32, Physical>,
        buffer: &WlBuffer,
    ) -> Result<(), Box<dyn Error>> {
        // Create and bind an offscreen render buffer.
        let buffer_dimensions = renderer::buffer_dimensions(buffer).unwrap();
        let offscreen_buffer: Gles2Renderbuffer = self.gles2.create_buffer(buffer_dimensions)?;
        self.gles2.bind(offscreen_buffer)?;

        let output = windows.output();
        let output_size = output.physical_resolution();
        let transform = output.orientation().output_transform();

        // Calculate drawing area after output transform.
        let damage = transform.transform_rect_in(region, &output_size);

        // Collect textures for rendering.
        let textures = windows.textures(&mut self.gles2, &mut self.graphics);

        // Initialize the buffer to our clear color.
        let mut frame = self.gles2.render(output_size, transform)?;
        frame.clear(CLEAR_COLOR, &[damage])?;

        // Render everything to the offscreen buffer.
        utils::draw_render_elements(&mut frame, 1., textures, &[damage])?;

        // Ensure rendering was fully completed.
        frame.finish()?;

        // Copy offscreen buffer's content to the SHM buffer.
        shm::with_buffer_contents_mut(buffer, |shm_buffer, shm_len, buffer_data| {
            // Ensure SHM buffer is in an acceptable format.
            if buffer_data.format != wl_shm::Format::Argb8888
                || buffer_data.stride != region.size.w * 4
                || buffer_data.height != region.size.h
                || shm_len as i32 != buffer_data.stride * buffer_data.height
            {
                return Err::<(), Box<dyn Error>>("Invalid buffer format".into());
            }

            // Copy framebuffer data to the SHM buffer.
            self.gles2.with_context(|gl| unsafe {
                gl.ReadPixels(
                    region.loc.x,
                    region.loc.y,
                    region.size.w,
                    region.size.h,
                    ffi::RGBA,
                    ffi::UNSIGNED_BYTE,
                    shm_buffer.cast(),
                );
            })?;

            // Convert OpenGL's RGBA to ARGB.
            for i in 0..(region.size.w * region.size.h) as usize {
                unsafe {
                    let src = shm_buffer.offset(i as isize * 4);
                    let dst = shm_buffer.offset(i as isize * 4 + 2);
                    ptr::swap(src, dst);
                }
            }

            Ok(())
        })?
    }

    /// Default dma surface feedback.
    fn default_dmabuf_feedback(&self) -> Result<DmabufFeedback, Box<dyn Error>> {
        // Get planes for the DRM surface.
        let surface = self.drm_compositor.surface();
        let planes = surface.planes()?;

        // Get supported plane formats.
        let primary_formats = surface.supported_formats(planes.primary.handle)?;
        let overlay_planes = planes.overlay.iter();
        let overlay_formats =
            overlay_planes.flat_map(|plane| surface.supported_formats(plane.handle)).flatten();

        // Setup feedback builder.
        let gbm_id = self.gbm.dev_id()?;
        let dmabuf_formats = Bind::<Dmabuf>::supported_formats(&self.gles2).unwrap();
        let feedback_builder = DmabufFeedbackBuilder::new(gbm_id, dmabuf_formats);

        // Create default feedback preference.
        let surface_id = surface.device_fd().dev_id()?;
        let flags = Some(TrancheFlags::Scanout);
        let feedback = feedback_builder
            // Ideally pick a format which can be scanned out on an overlay plane.
            .add_preference_tranche(surface_id, flags, overlay_formats)
            // Fallback to primary formats, still supporting direct scanout in fullscreen.
            .add_preference_tranche(surface_id, flags, primary_formats)
            .build()?;

        Ok(feedback)
    }
}

/// DRM compositor type alias.
type DrmCompositor =
    SmithayDrmCompositor<GbmAllocator<DrmDeviceFd>, GbmDevice<DrmDeviceFd>, (), DrmDeviceFd>;
