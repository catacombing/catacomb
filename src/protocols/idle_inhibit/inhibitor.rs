//! idle-inhibit inhibitor.

use _idle_inhibit::zwp_idle_inhibitor_v1::{Request, ZwpIdleInhibitorV1};
use smithay::reexports::wayland_protocols::wp::idle_inhibit::zv1::server as _idle_inhibit;
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::reexports::wayland_server::{Client, DataInit, Dispatch, DisplayHandle};

use crate::protocols::idle_inhibit::{IdleInhibitHandler, IdleInhibitManagerState};

pub struct IdleInhibitorState {
    pub surface: WlSurface,
}

impl<D> Dispatch<ZwpIdleInhibitorV1, IdleInhibitorState, D> for IdleInhibitManagerState
where
    D: Dispatch<ZwpIdleInhibitorV1, IdleInhibitorState>,
    D: IdleInhibitHandler,
    D: 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        _inhibitor: &ZwpIdleInhibitorV1,
        request: Request,
        data: &IdleInhibitorState,
        _display: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            Request::Destroy => state.uninhibit(&data.surface),
            _ => unreachable!(),
        }
    }
}
