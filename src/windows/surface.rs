//! Window surfaces.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Weak;
use std::sync::Mutex;

use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor;
use smithay::wayland::shell::wlr_layer::{
    Anchor, ExclusiveZone, LayerSurface, LayerSurfaceAttributes, LayerSurfaceState,
};
use smithay::wayland::shell::xdg::{
    PopupState, PopupSurface, SurfaceCachedState, ToplevelState, ToplevelSurface,
    XdgPopupSurfaceData, XdgToplevelSurfaceData, XdgToplevelSurfaceRoleAttributes,
};

use crate::protocols::session_lock::surface::{LockSurface, LockSurfaceState};
use crate::windows::Window;

/// Common surface functionality.
pub trait Surface {
    /// Surface state type.
    type State;

    /// Get underlying Wayland surface.
    fn surface(&self) -> &WlSurface;

    /// Check if the window has been closed.
    fn alive(&self) -> bool;

    /// Request application shutdown.
    fn send_close(&self);

    /// Send the initial configure.
    fn initial_configure(&self);

    /// Check whether the initial configure was already sent.
    fn initial_configure_sent(&self) -> bool;

    /// Update surface state.
    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F);

    /// Update surface's dimensions.
    fn resize(&self, size: Size<i32, Logical>);

    /// Window's acknowledged size.
    fn acked_size(&self) -> Size<i32, Logical>;

    /// Geometry of the window's visible bounds.
    fn geometry(&self) -> Option<Rectangle<i32, Logical>> {
        compositor::with_states(self.surface(), |states| {
            states.cached_state.current::<SurfaceCachedState>().geometry
        })
    }
}

impl Surface for ToplevelSurface {
    type State = ToplevelState;

    fn surface(&self) -> &WlSurface {
        self.wl_surface()
    }

    fn alive(&self) -> bool {
        self.alive()
    }

    fn send_close(&self) {
        self.send_close();
    }

    fn initial_configure(&self) {
        if !self.initial_configure_sent() {
            self.send_configure();
        }
    }

    fn initial_configure_sent(&self) -> bool {
        compositor::with_states(self.surface(), |states| {
            let surface_data = states.data_map.get::<XdgToplevelSurfaceData>().unwrap();
            surface_data.lock().unwrap().initial_configure_sent
        })
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.with_pending_state(f);

        // Ignore configures before the initial one.
        if self.initial_configure_sent() {
            self.send_configure();
        }
    }

    fn resize(&self, size: Size<i32, Logical>) {
        self.set_state(|state| {
            state.size = Some(size);
        });
    }

    fn acked_size(&self) -> Size<i32, Logical> {
        compositor::with_states(self.surface(), |states| {
            let attributes = states
                .data_map
                .get::<Mutex<XdgToplevelSurfaceRoleAttributes>>()
                .and_then(|attributes| attributes.lock().ok());

            attributes.and_then(|attributes| attributes.current.size)
        })
        .unwrap_or_default()
    }
}

impl Surface for PopupSurface {
    type State = PopupState;

    fn surface(&self) -> &WlSurface {
        self.wl_surface()
    }

    fn alive(&self) -> bool {
        self.alive()
    }

    fn send_close(&self) {}

    fn initial_configure(&self) {
        if self.initial_configure_sent() {
            return;
        }

        let _ = self.send_configure();
    }

    fn initial_configure_sent(&self) -> bool {
        compositor::with_states(self.surface(), |states| {
            let surface_data = states.data_map.get::<XdgPopupSurfaceData>().unwrap();
            surface_data.lock().unwrap().initial_configure_sent
        })
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.with_pending_state(f);

        // Ignore configures before the initial one.
        if self.initial_configure_sent() {
            let _ = self.send_configure();
        }
    }

    fn resize(&self, _size: Size<i32, Logical>) {}

    fn acked_size(&self) -> Size<i32, Logical> {
        self.with_pending_state(|state| state.positioner.rect_size)
    }
}

#[derive(Debug)]
pub struct CatacombLayerSurface {
    pub exclusive_zone: ExclusiveZone,
    pub anchor: Anchor,
    surface: LayerSurface,
}

impl From<LayerSurface> for CatacombLayerSurface {
    fn from(surface: LayerSurface) -> Self {
        Self { surface, exclusive_zone: Default::default(), anchor: Default::default() }
    }
}

impl Surface for CatacombLayerSurface {
    type State = LayerSurfaceState;

    fn surface(&self) -> &WlSurface {
        self.surface.wl_surface()
    }

    fn alive(&self) -> bool {
        self.surface.alive()
    }

    fn send_close(&self) {
        self.surface.send_close();
    }

    fn initial_configure(&self) {
        if self.initial_configure_sent() {
            return;
        }

        self.send_configure();
    }

    fn initial_configure_sent(&self) -> bool {
        compositor::with_states(self.surface(), |states| {
            let surface_data = states.data_map.get::<Mutex<LayerSurfaceAttributes>>().unwrap();
            surface_data.lock().unwrap().initial_configure_sent
        })
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.surface.with_pending_state(f);

        // Ignore configures before the initial one.
        if self.initial_configure_sent() {
            self.surface.send_configure();
        }
    }

    fn resize(&self, size: Size<i32, Logical>) {
        self.set_state(|state| {
            state.size = Some(size);
        });
    }

    fn acked_size(&self) -> Size<i32, Logical> {
        compositor::with_states(self.surface(), |states| {
            let attributes = states
                .data_map
                .get::<Mutex<LayerSurfaceAttributes>>()
                .and_then(|attributes| attributes.lock().ok());

            attributes.and_then(|attributes| attributes.current.size)
        })
        .unwrap_or_default()
    }

    fn geometry(&self) -> Option<Rectangle<i32, Logical>> {
        None
    }
}

impl Deref for CatacombLayerSurface {
    type Target = LayerSurface;

    fn deref(&self) -> &Self::Target {
        &self.surface
    }
}

impl Surface for LockSurface {
    type State = LockSurfaceState;

    fn surface(&self) -> &WlSurface {
        self.wl_surface()
    }

    fn alive(&self) -> bool {
        self.alive()
    }

    fn send_close(&self) {
        unreachable!("attempted to kill lock surface");
    }

    fn initial_configure(&self) {
        // Initial configure is sent immediately after
        // ext_session_lock_surface_v1 is bound.
    }

    fn initial_configure_sent(&self) -> bool {
        // See `initial_configure`.
        true
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.with_pending_state(f);
        self.send_configure();
    }

    fn resize(&self, size: Size<i32, Logical>) {
        self.set_state(|state| {
            let size = (size.w as u32, size.h as u32);
            state.size = Some(size.into());
        });
    }

    fn acked_size(&self) -> Size<i32, Logical> {
        let current_state = self.current_state();
        let size = current_state.size.unwrap_or_default();
        (size.w as i32, size.h as i32).into()
    }

    fn geometry(&self) -> Option<Rectangle<i32, Logical>> {
        None
    }
}

/// Surface with offset from its window origin.
pub struct OffsetSurface {
    pub toplevel: Option<OffsetSurfaceToplevel>,
    pub offset: Point<i32, Logical>,
    pub surface: WlSurface,
}

impl OffsetSurface {
    pub fn new(surface: WlSurface, offset: Point<i32, Logical>) -> Self {
        Self { surface, offset, toplevel: None }
    }
}

/// Focusable surface toplevel kinds.
pub enum OffsetSurfaceToplevel {
    Layout((Weak<RefCell<Window>>, Option<String>)),
    Layer(WlSurface),
}
