//! Drawing utilities.

use std::ops::Deref;
use std::rc::Rc;

use smithay::backend::renderer::gles2::{ffi, Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::{self, Frame};
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
use smithay::utils::{Buffer as BufferSpace, Logical, Physical, Point, Rectangle, Size, Transform};
use smithay::wayland::compositor::{
    BufferAssignment, Damage as SurfaceDamage, RectangleKind, SurfaceAttributes,
};

use crate::geometry::Vector;
use crate::output::Canvas;

/// Maximum buffer age before damage information is discarded.
pub const MAX_DAMAGE_AGE: usize = 2;

/// Color of the hovered overview tiling location highlight.
const ACTIVE_DROP_TARGET_RGBA: [u8; 4] = [128, 128, 128, 128];

/// Color for the damage debug overlay.
const DAMAGE_DEBUG_RGBA: [u8; 4] = [255, 0, 255, 64];

/// Color of the overview tiling location highlight.
const DROP_TARGET_RGBA: [u8; 4] = [128, 128, 128, 64];

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
#[derive(Clone, Debug)]
pub struct Texture {
    location: Point<i32, Logical>,
    texture: Rc<Gles2Texture>,
    size: Size<i32, Logical>,
    transform: Transform,
    scale: i32,
}

impl Texture {
    pub fn new(texture: Rc<Gles2Texture>, size: impl Into<Size<i32, Logical>>) -> Self {
        Self {
            size: size.into(),
            scale: 1,
            texture,
            transform: Default::default(),
            location: Default::default(),
        }
    }

    pub fn from_surface(
        texture: Rc<Gles2Texture>,
        location: impl Into<Point<i32, Logical>>,
        buffer: &CatacombSurfaceData,
    ) -> Self {
        Self {
            transform: buffer.transform,
            location: location.into(),
            scale: buffer.scale,
            size: buffer.size,
            texture,
        }
    }

    /// Create a texture from an RGBA buffer.
    pub fn from_buffer(
        renderer: &mut Gles2Renderer,
        buffer: &[u8],
        width: i32,
        height: i32,
    ) -> Self {
        assert!(buffer.len() as i32 >= width * height * 4);

        let texture_id = renderer
            .with_context(|gl| unsafe {
                let mut tex = 0;
                gl.GenTextures(1, &mut tex);
                gl.BindTexture(ffi::TEXTURE_2D, tex);
                gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_S, ffi::CLAMP_TO_EDGE as i32);
                gl.TexParameteri(ffi::TEXTURE_2D, ffi::TEXTURE_WRAP_T, ffi::CLAMP_TO_EDGE as i32);
                gl.TexImage2D(
                    ffi::TEXTURE_2D,
                    0,
                    ffi::RGBA as i32,
                    width,
                    height,
                    0,
                    ffi::RGBA,
                    ffi::UNSIGNED_BYTE,
                    buffer.as_ptr().cast(),
                );
                gl.BindTexture(ffi::TEXTURE_2D, 0);

                tex
            })
            .expect("create texture");

        let texture =
            unsafe { Gles2Texture::from_raw(renderer, texture_id, (width, height).into()) };

        Texture::new(Rc::new(texture), (width, height))
    }

    /// Render the texture at the specified location.
    ///
    /// Bounds is an absolute rectangle to which the window's rendering will be
    /// clipped. Passing `None` will not clamp the window.
    pub fn draw_at(
        &self,
        frame: &mut Gles2Frame,
        canvas: &Canvas,
        location: Point<i32, Logical>,
        bounds: Rectangle<i32, Logical>,
        scale: f64,
        damage: impl Into<Option<Rectangle<i32, Physical>>>,
    ) {
        // Convert destination bounds to texture-relative rectangle.
        let mut scaled_bounds = bounds;
        scaled_bounds.loc -= location;
        scaled_bounds.size = scaled_bounds.size.scale(1. / scale).max((1, 1));
        scaled_bounds.loc = scaled_bounds.loc.scale(1. / scale);
        scaled_bounds.loc -= self.location;

        // Calculate source texture rectangle.
        let mut src = Rectangle::from_loc_and_size((0, 0), self.size);
        src = match src.intersection(scaled_bounds) {
            Some(src) => src,
            // Skip rendering when completely outside of bounds.
            None => return,
        };
        let src_buffer = src.to_buffer(self.scale, self.transform, &self.size);

        // Calculate destination rectangle.
        let dst_location = location + (src.loc + self.location).scale(scale);
        let mut dst = Rectangle::from_loc_and_size(dst_location, src.size.scale(scale));
        dst = match dst.intersection(bounds) {
            Some(dst) => dst,
            // Skip rendering when completely outside of bounds.
            None => return,
        };
        let dst_physical = dst.to_physical(canvas.scale());

        // Calculate surface damage.
        let full_damage = Rectangle::from_loc_and_size((0, 0), dst_physical.size);
        let surface_damage = match damage.into() {
            Some(mut damage) => {
                damage.loc -= dst_physical.loc;
                full_damage.intersection(damage)
            },
            None => Some(full_damage),
        };

        // Draw the damaged surface.
        if let Some(surface_damage) = surface_damage {
            let _ = frame.render_texture_from_to(
                &self.texture,
                src_buffer.to_f64(),
                dst_physical,
                &[surface_damage],
                self.transform,
                1.,
            );
        }
    }
}

/// Grahpics texture cache.
#[derive(Debug)]
pub struct Graphics {
    pub active_drop_target: Texture,
    pub damage_debug: Texture,
    pub drop_target: Texture,
}

impl Graphics {
    pub fn new(renderer: &mut Gles2Renderer) -> Self {
        let active_drop_target = Texture::from_buffer(renderer, &ACTIVE_DROP_TARGET_RGBA, 1, 1);
        let damage_debug = Texture::from_buffer(renderer, &DAMAGE_DEBUG_RGBA, 1, 1);
        let drop_target = Texture::from_buffer(renderer, &DROP_TARGET_RGBA, 1, 1);
        Self { active_drop_target, damage_debug, drop_target }
    }
}

/// Surface data store.
pub struct CatacombSurfaceData {
    pub opaque_region: Vec<(RectangleKind, Rectangle<i32, Logical>)>,
    pub texture: Option<Texture>,
    pub size: Size<i32, Logical>,
    pub buffer: Option<Buffer>,
    pub transform: Transform,
    pub damage: Damage,
    pub scale: i32,
}

impl Default for CatacombSurfaceData {
    fn default() -> Self {
        Self {
            scale: 1,
            opaque_region: Default::default(),
            transform: Default::default(),
            texture: Default::default(),
            buffer: Default::default(),
            damage: Default::default(),
            size: Default::default(),
        }
    }
}

impl CatacombSurfaceData {
    pub fn new() -> Self {
        Self::default()
    }

    /// Handle buffer creation/removal.
    pub fn update_buffer(
        &mut self,
        attributes: &mut SurfaceAttributes,
        assignment: BufferAssignment,
    ) {
        match assignment {
            BufferAssignment::NewBuffer(buffer) => {
                self.size = renderer::buffer_dimensions(&buffer)
                    .unwrap_or_default()
                    .to_logical(self.scale, self.transform);
                self.transform = attributes.buffer_transform.into();
                self.scale = attributes.buffer_scale;
                self.buffer = Some(Buffer(buffer));
                self.texture = None;
            },
            BufferAssignment::Removed => *self = Self::default(),
        }

        // Store pending damage.
        for damage in attributes.damage.drain(..) {
            self.add_damage(damage);
        }
    }

    /// Add new surface damage.
    fn add_damage(&mut self, damage: SurfaceDamage) {
        let (buffer, logical) = match damage {
            SurfaceDamage::Buffer(buffer) => {
                let buffer_size = self.size.to_buffer(self.scale, self.transform);
                let logical = buffer.to_logical(self.scale, self.transform, &buffer_size);
                (buffer, logical)
            },
            SurfaceDamage::Surface(logical) => {
                let buffer = logical.to_buffer(self.scale, self.transform, &self.size);
                (buffer, logical)
            },
        };
        self.damage.logical.push(logical);
        self.damage.buffer.push(buffer);
    }
}

/// Container for automatically releasing a buffer on drop.
pub struct Buffer(WlBuffer);

impl Drop for Buffer {
    fn drop(&mut self) {
        self.0.release();
    }
}

impl Deref for Buffer {
    type Target = WlBuffer;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Pending buffer damage.
#[derive(Default)]
pub struct Damage {
    buffer: Vec<Rectangle<i32, BufferSpace>>,
    logical: Vec<Rectangle<i32, Logical>>,
}

impl Damage {
    /// Get pending buffer damage.
    pub fn buffer(&self) -> &[Rectangle<i32, BufferSpace>] {
        self.buffer.as_slice()
    }

    /// Get buffer's damage in logical coordinates.
    pub fn logical(&self) -> &[Rectangle<i32, Logical>] {
        &self.logical
    }

    /// Clear all damage.
    pub fn clear(&mut self) {
        self.logical.clear();
        self.buffer.clear();
    }
}
