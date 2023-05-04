//! idle-inhibit protocol.

use _idle_inhibit::zwp_idle_inhibit_manager_v1::{Request, ZwpIdleInhibitManagerV1};
use _idle_inhibit::zwp_idle_inhibitor_v1::ZwpIdleInhibitorV1;
use smithay::reexports::wayland_protocols::wp::idle_inhibit::zv1::server as _idle_inhibit;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New,
};

use crate::protocols::idle_inhibit::inhibitor::IdleInhibitorState;

pub mod inhibitor;

const MANAGER_VERSION: u32 = 1;

pub struct IdleInhibitManagerState;

impl IdleInhibitManagerState {
    pub fn new<D>(display: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<ZwpIdleInhibitManagerV1, ()>,
        D: Dispatch<ZwpIdleInhibitManagerV1, ()>,
        D: Dispatch<ZwpIdleInhibitorV1, IdleInhibitorState>,
        D: IdleInhibitHandler,
        D: 'static,
    {
        display.create_global::<D, ZwpIdleInhibitManagerV1, _>(MANAGER_VERSION, ());

        Self
    }
}

impl<D> GlobalDispatch<ZwpIdleInhibitManagerV1, (), D> for IdleInhibitManagerState
where
    D: GlobalDispatch<ZwpIdleInhibitManagerV1, ()>,
    D: Dispatch<ZwpIdleInhibitManagerV1, ()>,
    D: Dispatch<ZwpIdleInhibitorV1, IdleInhibitorState>,
    D: IdleInhibitHandler,
    D: 'static,
{
    fn bind(
        _state: &mut D,
        _display: &DisplayHandle,
        _client: &Client,
        manager: New<ZwpIdleInhibitManagerV1>,
        _manager_state: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(manager, ());
    }
}

impl<D> Dispatch<ZwpIdleInhibitManagerV1, (), D> for IdleInhibitManagerState
where
    D: GlobalDispatch<ZwpIdleInhibitManagerV1, ()>,
    D: Dispatch<ZwpIdleInhibitManagerV1, ()>,
    D: Dispatch<ZwpIdleInhibitorV1, IdleInhibitorState>,
    D: IdleInhibitHandler,
    D: 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _manager: &ZwpIdleInhibitManagerV1,
        request: Request,
        _data: &(),
        _display: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            Request::CreateInhibitor { id, surface } => {
                state.inhibit(&surface);
                data_init.init(id, IdleInhibitorState { surface });
            },
            Request::Destroy => (),
            _ => unreachable!(),
        }
    }
}

/// Handler trait for idle-inhibit.
pub trait IdleInhibitHandler {
    /// Enable idle inhibition for the output of the provided surface.
    fn inhibit(&mut self, surface: &WlSurface);

    /// Stop inhibition for the provided surface.
    fn uninhibit(&mut self, surface: &WlSurface);
}

#[allow(missing_docs)]
#[macro_export]
macro_rules! delegate_idle_inhibit {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::wp::idle_inhibit::zv1::server::zwp_idle_inhibit_manager_v1::ZwpIdleInhibitManagerV1: ()
        ] => $crate::protocols::idle_inhibit::IdleInhibitManagerState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::wp::idle_inhibit::zv1::server::zwp_idle_inhibit_manager_v1::ZwpIdleInhibitManagerV1: ()
        ] => $crate::protocols::idle_inhibit::IdleInhibitManagerState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols::wp::idle_inhibit::zv1::server::zwp_idle_inhibitor_v1::ZwpIdleInhibitorV1: $crate::protocols::idle_inhibit::inhibitor::IdleInhibitorState
        ] => $crate::protocols::idle_inhibit::IdleInhibitManagerState);
    };
}
