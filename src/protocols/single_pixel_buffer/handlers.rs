use smithay::reexports::wayland_server::protocol::wl_buffer::{self, WlBuffer};
use smithay::reexports::wayland_server::{
    Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New,
};
use smithay::wayland::buffer::BufferHandler;
use smithay::reexports::wayland_protocols::wp::single_pixel_buffer::v1::server::wp_single_pixel_buffer_manager_v1::{
    self, WpSinglePixelBufferManagerV1,
};

use crate::protocols::single_pixel_buffer::{SinglePixelBufferState, SinglePixelBufferUserData};

impl<D> GlobalDispatch<WpSinglePixelBufferManagerV1, (), D> for SinglePixelBufferState
where
    D: GlobalDispatch<WpSinglePixelBufferManagerV1, ()>,
    D: Dispatch<WpSinglePixelBufferManagerV1, ()>,
    D: 'static,
{
    fn bind(
        _state: &mut D,
        _dh: &DisplayHandle,
        _client: &Client,
        resource: New<WpSinglePixelBufferManagerV1>,
        _global_data: &(),
        data_init: &mut DataInit<'_, D>,
    ) {
        data_init.init(resource, ());
    }
}

impl<D> Dispatch<WpSinglePixelBufferManagerV1, (), D> for SinglePixelBufferState
where
    D: Dispatch<WpSinglePixelBufferManagerV1, ()>,
    D: Dispatch<WlBuffer, SinglePixelBufferUserData>,
    D: 'static,
{
    fn request(
        _state: &mut D,
        _client: &Client,
        _manager: &WpSinglePixelBufferManagerV1,
        request: wp_single_pixel_buffer_manager_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            wp_single_pixel_buffer_manager_v1::Request::CreateU32RgbaBuffer {
                id: buffer,
                r,
                g,
                b,
                a,
            } => {
                data_init.init(buffer, SinglePixelBufferUserData { r, g, b, a });
            },
            wp_single_pixel_buffer_manager_v1::Request::Destroy => {},
            _ => unimplemented!(),
        }
    }
}

impl<D> Dispatch<WlBuffer, SinglePixelBufferUserData, D> for SinglePixelBufferState
where
    D: Dispatch<WlBuffer, SinglePixelBufferUserData>,
    D: BufferHandler,
{
    fn request(
        data: &mut D,
        _client: &Client,
        buffer: &wl_buffer::WlBuffer,
        request: wl_buffer::Request,
        _udata: &SinglePixelBufferUserData,
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            wl_buffer::Request::Destroy => {
                data.buffer_destroyed(buffer);
            },
            _ => unreachable!(),
        }
    }
}
