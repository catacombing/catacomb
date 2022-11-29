//! Window surfaces.

use std::ops::Deref;
use std::sync::Mutex;

use _decoration::zv1::server::zxdg_toplevel_decoration_v1::Mode as DecorationMode;
use smithay::reexports::wayland_protocols::xdg::decoration as _decoration;
use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::State;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{Logical, Point, Rectangle, Size};
use smithay::wayland::compositor;
use smithay::wayland::shell::wlr_layer::{
    Anchor, ExclusiveZone, LayerSurface, LayerSurfaceAttributes, LayerSurfaceState,
};
use smithay::wayland::shell::xdg::{
    PopupState, PopupSurface, SurfaceCachedState, ToplevelState, ToplevelSurface,
    XdgToplevelSurfaceRoleAttributes,
};

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

    /// Update surface state.
    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F);

    /// Send a configure for the latest window properties.
    fn reconfigure(&self, size: Size<i32, Logical>);

    /// Window's acknowledged size.
    fn acked_size(&self) -> Size<i32, Logical>;

    /// Geometry of the window's visible bounds.
    fn geometry(&self) -> Rectangle<i32, Logical> {
        compositor::with_states(self.surface(), |states| {
            states.cached_state.current::<SurfaceCachedState>().geometry
        })
        .unwrap_or_else(|| Rectangle::from_loc_and_size((0, 0), self.acked_size()))
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
        self.send_close()
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.with_pending_state(f);
        self.send_configure();
    }

    fn reconfigure(&self, size: Size<i32, Logical>) {
        self.set_state(|state| {
            state.size = Some(size);

            // Mark window as tiled, using maximized fallback if tiling is unsupported.
            if self.version() >= 2 {
                state.states.set(State::TiledBottom);
                state.states.set(State::TiledRight);
                state.states.set(State::TiledLeft);
                state.states.set(State::TiledTop);
            } else {
                state.states.set(State::Maximized);
            }

            // Always use server-side decoration.
            state.decoration_mode = Some(DecorationMode::ServerSide);
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

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.with_pending_state(f);
        let _ = self.send_configure();
    }

    fn reconfigure(&self, _size: Size<i32, Logical>) {
        let _ = self.send_configure();
    }

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
        self.surface.send_close()
    }

    fn set_state<F: FnMut(&mut Self::State)>(&self, f: F) {
        self.surface.with_pending_state(f);
        self.surface.send_configure();
    }

    fn reconfigure(&self, size: Size<i32, Logical>) {
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

    fn geometry(&self) -> Rectangle<i32, Logical> {
        Rectangle::from_loc_and_size((0, 0), self.acked_size())
    }
}

impl Deref for CatacombLayerSurface {
    type Target = LayerSurface;

    fn deref(&self) -> &Self::Target {
        &self.surface
    }
}

/// Surface with offset from its window origin.
pub struct OffsetSurface {
    pub offset: Point<i32, Logical>,
    pub surface: WlSurface,
}

impl OffsetSurface {
    pub fn new(surface: WlSurface, offset: Point<i32, Logical>) -> Self {
        Self { surface, offset }
    }
}
