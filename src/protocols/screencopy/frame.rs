//! wlr-screencopy frame.

use std::time::UNIX_EPOCH;

use smithay::reexports::wayland_protocols_wlr::screencopy::v1::server::zwlr_screencopy_frame_v1::{
    Flags, Request, ZwlrScreencopyFrameV1,
};
use smithay::reexports::wayland_server::{Client, DataInit, Dispatch, DisplayHandle};
use smithay::utils::{Physical, Rectangle};

use crate::protocols::screencopy::{ScreencopyHandler, ScreencopyManagerState};

pub struct ScreencopyFrameState {
    pub rect: Rectangle<i32, Physical>,
    pub overlay_cursor: bool,
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
        match request {
            Request::Copy { buffer } => {
                // Instruct compositor to copy frame into the buffer.
                if let Err(err) = state.copy(&buffer, data.rect, data.overlay_cursor) {
                    eprintln!("Screencopy failed: {err:?}");

                    // Mark frame as failed on copy failure.
                    frame.failed();
                    return;
                }

                // Notify client that buffer is ordinary.
                frame.flags(Flags::empty());

                // Notify client about successful copy.
                let now = UNIX_EPOCH.elapsed().unwrap();
                let secs = now.as_secs();
                frame.ready((secs >> 32) as u32, secs as u32, now.subsec_nanos());
            },
            Request::CopyWithDamage { buffer } => {
                let damage = match state.copy(&buffer, data.rect, data.overlay_cursor) {
                    Ok(damage) => damage,
                    Err(err) => {
                        eprintln!("Screencopy failed: {err:?}");

                        // Mark frame as failed on copy failure.
                        frame.failed();
                        return;
                    },
                };

                // Notify client that buffer is ordinary.
                frame.flags(Flags::empty());

                // Notify client about buffer damage.
                for Rectangle { loc, size } in damage {
                    frame.damage(loc.x as u32, loc.y as u32, size.w as u32, size.h as u32);
                }

                // Notify client about successful copy.
                let now = UNIX_EPOCH.elapsed().unwrap();
                let secs = now.as_secs();
                frame.ready((secs >> 32) as u32, secs as u32, now.subsec_nanos());
            },
            Request::Destroy => (),
            _ => unreachable!(),
        }
    }
}
