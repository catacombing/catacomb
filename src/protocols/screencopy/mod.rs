//! wlr-screencopy protocol.

use std::error::Error;

use _screencopy::zwlr_screencopy_frame_v1::ZwlrScreencopyFrameV1;
use _screencopy::zwlr_screencopy_manager_v1::{Request, ZwlrScreencopyManagerV1};
use smithay::backend::allocator::Fourcc;
use smithay::reexports::wayland_protocols_wlr::screencopy::v1::server as _screencopy;
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::reexports::wayland_server::protocol::wl_output::WlOutput;
use smithay::reexports::wayland_server::protocol::wl_shm;
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
};
use smithay::utils::{Physical, Rectangle, Size};

use crate::protocols::screencopy::frame::ScreencopyFrameState;

pub mod frame;

const MANAGER_VERSION: u32 = 3;

pub struct ScreencopyManagerState;

impl ScreencopyManagerState {
    pub fn new<D>(display: &DisplayHandle) -> Self
    where
        D: GlobalDispatch<ZwlrScreencopyManagerV1, ()>,
        D: Dispatch<ZwlrScreencopyManagerV1, ()>,
        D: Dispatch<ZwlrScreencopyFrameV1, ScreencopyFrameState>,
        D: ScreencopyHandler,
        D: 'static,
    {
        display.create_global::<D, ZwlrScreencopyManagerV1, _>(MANAGER_VERSION, ());

        Self
    }
}

impl<D> GlobalDispatch<ZwlrScreencopyManagerV1, (), D> for ScreencopyManagerState
where
    D: GlobalDispatch<ZwlrScreencopyManagerV1, ()>,
    D: Dispatch<ZwlrScreencopyManagerV1, ()>,
    D: Dispatch<ZwlrScreencopyFrameV1, ScreencopyFrameState>,
    D: ScreencopyHandler,
    D: 'static,
{
    fn bind(
        _state: &mut D,
        _display: &DisplayHandle,
        _client: &Client,
        manager: New<ZwlrScreencopyManagerV1>,
        _manager_state: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(manager, ());
    }
}

impl<D> Dispatch<ZwlrScreencopyManagerV1, (), D> for ScreencopyManagerState
where
    D: GlobalDispatch<ZwlrScreencopyManagerV1, ()>,
    D: Dispatch<ZwlrScreencopyManagerV1, ()>,
    D: Dispatch<ZwlrScreencopyFrameV1, ScreencopyFrameState>,
    D: ScreencopyHandler,
    D: 'static,
{
    fn request(
        state: &mut D,
        _client: &Client,
        manager: &ZwlrScreencopyManagerV1,
        request: Request,
        _data: &(),
        _display: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        let (frame, overlay_cursor, rect) = match request {
            Request::CaptureOutput { frame, overlay_cursor, output } => {
                let rect = Rectangle::from_loc_and_size((0, 0), state.output_size(&output));
                (frame, overlay_cursor, rect)
            },
            Request::CaptureOutputRegion { frame, overlay_cursor, output, x, y, width, height } => {
                let output_size = state.output_size(&output);
                let rect = Rectangle::from_loc_and_size((x, y), (width, height));

                // Clamp captured region to the output.
                let rect = rect
                    .intersection(Rectangle::from_loc_and_size((0, 0), output_size))
                    .unwrap_or_default();

                (frame, overlay_cursor, rect)
            },
            Request::Destroy => return,
            _ => unreachable!(),
        };

        // Create the frame.
        let overlay_cursor = overlay_cursor != 0;
        let frame = data_init.init(frame, ScreencopyFrameState { overlay_cursor, rect });

        // Send desired SHM buffer parameters.
        frame.buffer(
            wl_shm::Format::Argb8888,
            rect.size.w as u32,
            rect.size.h as u32,
            rect.size.w as u32 * 4,
        );

        if manager.version() >= 3 {
            // Send desired DMA buffer parameters.
            frame.linux_dmabuf(Fourcc::Argb8888 as u32, rect.size.w as u32, rect.size.h as u32);

            // Notify client that all supported buffers were enumerated.
            frame.buffer_done();
        }
    }
}

/// Handler trait for wlr-screencopy.
pub trait ScreencopyHandler {
    /// Get the size of an output.
    fn output_size(&mut self, output: &WlOutput) -> Size<i32, Physical>;

    /// Copy a region from the framebuffer into the supplied buffer.
    fn copy(
        &mut self,
        buffer: &WlBuffer,
        region: Rectangle<i32, Physical>,
        overlay_cursor: bool,
    ) -> Result<Vec<Rectangle<i32, Physical>>, Box<dyn Error>>;
}

#[allow(missing_docs)]
#[macro_export]
macro_rules! delegate_screencopy_manager {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::screencopy::v1::server::zwlr_screencopy_manager_v1::ZwlrScreencopyManagerV1: ()
        ] => $crate::protocols::screencopy::ScreencopyManagerState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::screencopy::v1::server::zwlr_screencopy_manager_v1::ZwlrScreencopyManagerV1: ()
        ] => $crate::protocols::screencopy::ScreencopyManagerState);

        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            smithay::reexports::wayland_protocols_wlr::screencopy::v1::server::zwlr_screencopy_frame_v1::ZwlrScreencopyFrameV1: $crate::protocols::screencopy::frame::ScreencopyFrameState
        ] => $crate::protocols::screencopy::ScreencopyManagerState);
    };
}
