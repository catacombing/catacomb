use std::cell::RefCell;
use std::env;
use std::ops::Deref;
use std::rc::Rc;
use std::time::{Duration, Instant};

use slog::{o, Drain, Logger};
use smithay::backend::input::{Event, InputBackend, InputEvent, KeyboardKeyEvent};
use smithay::backend::renderer::gles2::Gles2Texture;
use smithay::backend::renderer::{
    self, BufferType, Frame, ImportAll, ImportDma, ImportEgl, Transform,
};
use smithay::backend::winit;
use smithay::reexports::calloop::generic::Generic;
use smithay::reexports::calloop::{EventLoop, Interest, Mode, PostAction};
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::Display;
use smithay::utils::{Logical, Point};
use smithay::wayland::compositor::{
    self, BufferAssignment, Damage, SubsurfaceCachedState, SurfaceAttributes, TraversalAction,
};
use smithay::wayland::seat::{KeyboardHandle, Seat, XkbConfig};
use smithay::wayland::shell::xdg::{self as xdg_shell, ToplevelSurface, XdgRequest};
use smithay::wayland::{data_device, dmabuf, shm, SERIAL_COUNTER};

struct State {
    keyboard: KeyboardHandle,
    terminated: bool,
}

struct Window {
    surface: ToplevelSurface,
    location: Point<i32, Logical>,
}

impl Window {
    fn new(surface: ToplevelSurface, location: Point<i32, Logical>) -> Self {
        Window { surface, location }
    }
}

#[derive(Default)]
struct SurfaceData {
    texture: Option<BufferTexture>,
    buffer: Option<WlBuffer>,
}

struct BufferTexture {
    texture: Gles2Texture,
    // NOTE: Buffer is held for Drop impl.
    #[allow(unused)]
    buffer: Option<UnreleasedBuffer>,
}

/// Container for automatically releasing the wrapped buffer on drop.
struct UnreleasedBuffer(WlBuffer);

impl Deref for UnreleasedBuffer {
    type Target = WlBuffer;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for UnreleasedBuffer {
    fn drop(&mut self) {
        self.0.release();
    }
}

fn main() {
    let log = Logger::root(slog_async::Async::default(slog_term::term_full().fuse()).fuse(), o!());
    let _guard = slog_scope::set_global_logger(log.clone());
    slog_stdlog::init().expect("Could not setup log backend");

    let mut display = Display::new();

    let (graphics, mut input) = winit::init(log.clone()).expect("init winit");
    let graphics = Rc::new(RefCell::new(graphics));

    // Setup hardware acceleration.
    if graphics.borrow_mut().renderer().bind_wl_display(&display).is_ok() {
        let formats: Vec<_> = graphics.borrow_mut().renderer().dmabuf_formats().cloned().collect();
        let graphics = graphics.clone();
        dmabuf::init_dmabuf_global(
            &mut display,
            formats,
            move |buffer, _| graphics.borrow_mut().renderer().import_dmabuf(buffer).is_ok(),
            log.clone(),
        );
    }

    let mut event_loop: EventLoop<'_, State> = EventLoop::try_new().expect("event loop");

    shm::init_shm_global(&mut display, Vec::new(), None);

    // Create the compositor.
    compositor::compositor_init(
        &mut display,
        |surface, _| {
            if !compositor::is_sync_subsurface(&surface) {
                compositor::with_surface_tree_upward(
                    &surface,
                    (),
                    |_, _, _| TraversalAction::DoChildren(()),
                    |_, surface_state, _| {
                        surface_state
                            .data_map
                            .insert_if_missing(|| RefCell::new(SurfaceData::default()));

                        let mut data = surface_state
                            .data_map
                            .get::<RefCell<SurfaceData>>()
                            .unwrap()
                            .borrow_mut();

                        let mut attributes =
                            surface_state.cached_state.current::<SurfaceAttributes>();

                        match attributes.buffer.take() {
                            Some(BufferAssignment::NewBuffer { buffer, .. }) => {
                                if let Some(old_buffer) = data.buffer.replace(buffer) {
                                    old_buffer.release();
                                }
                                data.texture = None;
                            },
                            // TODO: This never gets called, when does it happen?
                            //       Why do we not release the buffer here?
                            Some(BufferAssignment::Removed) => {
                                data.texture = None;
                                data.buffer = None;
                            },
                            None => (),
                        }
                    },
                    |_, _, _| true,
                );
            }
        },
        log.clone(),
    );

    let windows = Rc::new(RefCell::new(Vec::new()));

    {
        let windows = windows.clone();
        let _ = xdg_shell::xdg_shell_init(
            &mut display,
            move |event, mut data| match event {
                XdgRequest::NewToplevel { surface } => {
                    let state = data.get::<State>().unwrap();
                    if let Some(wl_surface) = surface.get_surface() {
                        state.keyboard.set_focus(Some(wl_surface), SERIAL_COUNTER.next_serial());
                    }

                    windows.borrow_mut().push(Window::new(surface, Point::from((0, 30))));
                },
                _ => println!("UNHANDLED EVENT: {:?}", event),
            },
            log.clone(),
        );
    }

    // Create our Wayland socket.
    let socket_name = display
        .add_socket_auto()
        .expect("wayland socket")
        .into_string()
        .expect("wayland socket name");
    env::set_var("WAYLAND_DISPLAY", &socket_name);
    println!("Wayland socket: {}", socket_name);

    // Initialize input.
    let seat_name = String::from("winit");
    let (mut seat, _) = Seat::new(&mut display, seat_name, log.clone());

    let keyboard = seat
        .add_keyboard(XkbConfig::default(), 200, 25, |seat, focused_surface| {
            data_device::set_data_device_focus(
                seat,
                focused_surface.and_then(|surface| surface.as_ref().client()),
            )
        })
        .expect("adding keyboard");

    let mut state = State { terminated: false, keyboard };

    // Subscribe to Wayland socket events.
    let display = Rc::new(RefCell::new(display));
    {
        let display = display.clone();
        let fd = display.borrow().get_poll_fd();
        event_loop
            .handle()
            .insert_source(Generic::from_fd(fd, Interest::READ, Mode::Level), move |_, _, state| {
                let mut display = display.borrow_mut();
                match display.dispatch(Duration::from_millis(0), state) {
                    Ok(_) => Ok(PostAction::Continue),
                    Err(e) => {
                        eprintln!("I/O error on the Wayland display: {}", e);
                        state.terminated = true;
                        Err(e)
                    },
                }
            })
            .expect("register wayland socket source");
    }

    let start_time = Instant::now();

    loop {
        if input
            .dispatch_new_events(|event| {
                let event = match event {
                    InputEvent::Keyboard { event, .. } => event,
                    _ => return,
                };

                let keycode = event.key_code();
                let key_state = event.state();
                let serial = SERIAL_COUNTER.next_serial();
                let time = Event::time(&event);

                state.keyboard.input(keycode, key_state, serial, time, |_, _| true);
            })
            .is_err()
        {
            eprintln!("input error");
            break;
        }

        graphics
            .borrow_mut()
            .render(|renderer, frame| {
                let _ = frame.clear([1., 0., 1., 1.]);

                for window in windows.borrow().iter() {
                    let wl_surface = match window.surface.get_surface() {
                        Some(surface) => surface,
                        None => continue,
                    };

                    compositor::with_surface_tree_upward(
                        wl_surface,
                        window.location,
                        |_, surface_state, location| {
                            let data = match surface_state.data_map.get::<RefCell<SurfaceData>>() {
                                Some(data) => data,
                                None => return TraversalAction::SkipChildren,
                            };
                            let mut data = data.borrow_mut();

                            // Use the subsurface's location as the origin for its children.
                            let mut location = *location;
                            if surface_state.role == Some("subsurface") {
                                let subsurface_state =
                                    surface_state.cached_state.current::<SubsurfaceCachedState>();
                                location += subsurface_state.location;
                            }

                            if data.texture.is_some() {
                                return TraversalAction::DoChildren(location);
                            }

                            let buffer = match data.buffer.take() {
                                Some(buffer) => UnreleasedBuffer(buffer),
                                None => return TraversalAction::SkipChildren,
                            };

                            let attributes =
                                surface_state.cached_state.current::<SurfaceAttributes>();
                            let damage: Vec<_> = attributes
                                .damage
                                .iter()
                                .map(|damage| match damage {
                                    Damage::Buffer(rect) => *rect,
                                    Damage::Surface(rect) => {
                                        rect.to_buffer(attributes.buffer_scale)
                                    },
                                })
                                .collect();

                            match renderer.import_buffer(&buffer, Some(surface_state), &damage) {
                                Some(Ok(texture)) => {
                                    let buffer = match renderer::buffer_type(&buffer) {
                                        Some(BufferType::Shm) => None,
                                        _ => Some(buffer),
                                    };
                                    data.texture = Some(BufferTexture { texture, buffer });

                                    TraversalAction::DoChildren(location)
                                },
                                _ => {
                                    eprintln!("unable to import buffer");
                                    TraversalAction::SkipChildren
                                },
                            }
                        },
                        |_, surface_state, location| {
                            let data = match surface_state.data_map.get::<RefCell<SurfaceData>>() {
                                Some(data) => data,
                                None => return,
                            };
                            let data = data.borrow_mut();

                            let texture = match &data.texture {
                                Some(texture) => texture,
                                None => return,
                            };

                            let attributes =
                                surface_state.cached_state.current::<SurfaceAttributes>();

                            // Apply subsurface offset to parent's origin.
                            let mut location = *location;
                            if surface_state.role == Some("subsurface") {
                                let subsurface_state =
                                    surface_state.cached_state.current::<SubsurfaceCachedState>();
                                location += subsurface_state.location;
                            }

                            let _ = frame.render_texture_at(
                                &texture.texture,
                                location.to_f64().to_physical(1.).to_i32_round(),
                                attributes.buffer_scale,
                                1.,
                                Transform::Normal,
                                1.,
                            );
                        },
                        |_, _, _| true,
                    );
                }
            })
            .expect("buffer swap");

        let time_passed = start_time.elapsed().as_millis() as u32;
        for window in windows.borrow().iter() {
            let wl_surface = match window.surface.get_surface() {
                Some(surface) => surface,
                None => continue,
            };

            compositor::with_surface_tree_downward(
                wl_surface,
                (),
                |_, _, _| TraversalAction::DoChildren(()),
                |_, surface_state, _| {
                    let mut attributes = surface_state.cached_state.current::<SurfaceAttributes>();
                    for callback in attributes.frame_callbacks.drain(..) {
                        callback.done(time_passed);
                    }
                },
                |_, _, _| true,
            );
        }
        display.borrow_mut().flush_clients(&mut state);

        // TODO: Timeout of 0ms here means entire vblank is spent snoozin?
        if event_loop.dispatch(Some(Duration::from_millis(0)), &mut state).is_err() {
            eprintln!("event loop error");
            break;
        }

        // TODO: Flushing twice before/after dispatch is likely so clients have as much time as
        // possible to react to the events?
        display.borrow_mut().flush_clients(&mut state);
    }
}
