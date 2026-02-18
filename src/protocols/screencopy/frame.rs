//! wlr-screencopy frame.

use smithay::reexports::wayland_protocols_wlr::screencopy::v1::server::zwlr_screencopy_frame_v1::{
    Request, ZwlrScreencopyFrameV1,
};
use smithay::reexports::wayland_server::{Client, DataInit, Dispatch, DisplayHandle};
use smithay::utils::{Physical, Rectangle};

use crate::protocols::screencopy::{ScreencopyHandler, ScreencopyManagerState};
use crate::udev::{CaptureFrame, CaptureRequest};

pub struct ScreencopyFrameState {
    pub rect: Rectangle<i32, Physical>,
    pub _overlay_cursor: bool,
}

impl<D> Dispatch<ZwlrScreencopyFrameV1, ScreencopyFrameState, D> for ScreencopyManagerState
where
    D: Dispatch<ZwlrScreencopyFrameV1, ScreencopyFrameState>,
    D: ScreencopyHandler,
    D: 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        frame: &ZwlrScreencopyFrameV1,
        request: Request,
        data: &ScreencopyFrameState,
        _display: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let (buffer, send_damage) = match request {
            Request::Copy { buffer } => (buffer, false),
            Request::CopyWithDamage { buffer } => (buffer, true),
            Request::Destroy => return,
            _ => unreachable!(),
        };

        state.frame(CaptureRequest::new(
            CaptureFrame::Screencopy(frame.clone()),
            buffer,
            data.rect,
            send_damage,
        ));
    }
}
