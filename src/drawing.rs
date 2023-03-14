//! Drawing utilities.

use std::ops::Deref;
use std::rc::Rc;

use smithay::backend::renderer::element::utils::{
    CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement,
};
use smithay::backend::renderer::element::{Element, Id, RenderElement, UnderlyingStorage};
use smithay::backend::renderer::gles2::{ffi, Gles2Frame, Gles2Renderer, Gles2Texture};
use smithay::backend::renderer::utils::{
    Buffer, CommitCounter, DamageTracker, DamageTrackerSnapshot,
};
use smithay::backend::renderer::{self, Frame, Renderer};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{
    Buffer as BufferSpace, Logical, Physical, Point, Rectangle, Scale, Size, Transform,
};
use smithay::wayland::compositor::{
    BufferAssignment, Damage as SurfaceDamage, RectangleKind, SurfaceAttributes,
};

use crate::geometry::SubtractRectFast;
use crate::output::{Canvas, GESTURE_HANDLE_HEIGHT};

/// Color of the hovered overview tiling location highlight.
const ACTIVE_DROP_TARGET_RGBA: [u8; 4] = [64, 64, 64, 128];

/// Color of the overview tiling location highlight.
const DROP_TARGET_RGBA: [u8; 4] = [32, 32, 32, 64];

/// Relative size of gesture notch to the handle's whole width/height.
const GESTURE_NOTCH_PERCENTAGE: f64 = 0.2;

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
#[derive(Clone, Debug)]
pub struct Texture {
    opaque_regions: Vec<Rectangle<i32, Physical>>,
    tracker: DamageTrackerSnapshot<i32, Physical>,
    location: Point<i32, Logical>,
    size: Size<i32, Logical>,
    buffer: Option<Buffer>,
    texture: Gles2Texture,
    transform: Transform,
    scale: i32,
    id: Id,
}

impl Texture {
    /// Create a texture from an OpenGL texture.
    pub fn new(texture: Gles2Texture, size: impl Into<Size<i32, Logical>>, opaque: bool) -> Self {
        // Ensure fully opaque textures are treated as such.
        let size = size.into();
        let opaque_regions = if opaque {
            vec![Rectangle::from_loc_and_size((0, 0), size).to_physical(1)]
        } else {
            Vec::new()
        };

        Self {
            opaque_regions,
            texture,
            size,
            tracker: DamageTrackerSnapshot::empty(),
            id: Id::new(),
            scale: 1,
            transform: Default::default(),
            location: Default::default(),
            buffer: Default::default(),
        }
    }

    /// Create a texture from a Wayland surface.
    pub fn from_surface(
        texture: Gles2Texture,
        location: impl Into<Point<i32, Logical>>,
        buffer: &CatacombSurfaceData,
        surface: &WlSurface,
    ) -> Self {
        let location = location.into();

        // Get surface's opaque region.
        let mut opaque_regions = Vec::new();
        for (kind, rect) in &buffer.opaque_region {
            let rect = rect.to_physical(buffer.scale);
            match kind {
                RectangleKind::Add => opaque_regions.push(rect),
                RectangleKind::Subtract => opaque_regions.subtract_rect(rect),
            }
        }

        Self {
            opaque_regions,
            location,
            texture,
            tracker: buffer.damage.tracker.snapshot(),
            buffer: buffer.buffer.clone(),
            transform: buffer.transform,
            scale: buffer.scale,
            id: surface.into(),
            size: buffer.size,
        }
    }

    /// Create a texture from an RGBA buffer.
    pub fn from_buffer(
        renderer: &mut Gles2Renderer,
        buffer: &[u8],
        width: i32,
        height: i32,
        opaque: bool,
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

        Texture::new(texture, (width, height), opaque)
    }
}

/// Newtype to implement element traits on `Rc<Texture>`.
#[derive(Clone, Debug)]
pub struct RenderTexture(Rc<Texture>);

impl RenderTexture {
    pub fn new(texture: Texture) -> Self {
        Self(Rc::new(texture))
    }
}

impl Deref for RenderTexture {
    type Target = Texture;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Element for RenderTexture {
    fn id(&self) -> &Id {
        &self.id
    }

    fn current_commit(&self) -> CommitCounter {
        self.tracker.current_commit()
    }

    fn src(&self) -> Rectangle<f64, BufferSpace> {
        let size = self.size.to_buffer(self.scale, self.transform).to_f64();
        Rectangle::from_loc_and_size((0., 0.), size)
    }

    fn geometry(&self, _scale: Scale<f64>) -> Rectangle<i32, Physical> {
        Rectangle::from_loc_and_size(self.location, self.size).to_physical(self.scale)
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        self.tracker.damage_since(commit).unwrap_or_else(|| {
            // Fallback to fully damage.
            vec![Rectangle::from_loc_and_size((0, 0), self.geometry(scale).size)]
        })
    }

    fn opaque_regions(&self, _scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        self.opaque_regions.clone()
    }
}

impl RenderElement<Gles2Renderer> for RenderTexture {
    fn draw<'a>(
        &self,
        frame: &mut Gles2Frame,
        src: Rectangle<f64, BufferSpace>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <Gles2Renderer as Renderer>::Error> {
        frame.render_texture_from_to(&self.texture, src, dst, damage, self.transform, 1.)
    }

    fn underlying_storage(&self, _renderer: &mut Gles2Renderer) -> Option<UnderlyingStorage> {
        self.buffer.clone().map(UnderlyingStorage::Wayland)
    }
}

/// Catacomb render element type.
#[derive(Debug)]
pub struct CatacombElement(
    CropRenderElement<RelocateRenderElement<RescaleRenderElement<RenderTexture>>>,
);

impl CatacombElement {
    pub fn add_element(
        textures: &mut Vec<CatacombElement>,
        texture: RenderTexture,
        location: impl Into<Point<i32, Physical>>,
        bounds: impl Into<Option<Rectangle<i32, Physical>>>,
        window_scale: impl Into<Option<f64>>,
    ) {
        let bounds = bounds
            .into()
            .unwrap_or_else(|| Rectangle::from_loc_and_size((0, 0), (i32::MAX, i32::MAX)));
        let window_scale = window_scale.into().unwrap_or(1.);
        let location = location.into();

        let rescaled_element =
            RescaleRenderElement::from_element(texture, (0, 0).into(), window_scale);
        let relocated_element =
            RelocateRenderElement::from_element(rescaled_element, location, Relocate::Relative);
        let cropped_element = CropRenderElement::from_element(relocated_element, 1., bounds);

        if let Some(cropped_element) = cropped_element {
            textures.push(Self(cropped_element));
        }
    }
}

impl Element for CatacombElement {
    fn id(&self) -> &Id {
        self.0.id()
    }

    fn current_commit(&self) -> CommitCounter {
        self.0.current_commit()
    }

    fn src(&self) -> Rectangle<f64, BufferSpace> {
        self.0.src()
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        self.0.geometry(scale)
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        self.0.damage_since(scale, commit)
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        self.0.opaque_regions(scale)
    }
}

impl RenderElement<Gles2Renderer> for CatacombElement {
    fn draw<'a>(
        &self,
        frame: &mut Gles2Frame,
        src: Rectangle<f64, BufferSpace>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <Gles2Renderer as Renderer>::Error> {
        self.0.draw(frame, src, dst, damage)
    }

    fn underlying_storage(&self, renderer: &mut Gles2Renderer) -> Option<UnderlyingStorage> {
        self.0.underlying_storage(renderer)
    }
}

/// Grahpics texture cache.
#[derive(Debug)]
pub struct Graphics {
    pub gesture_handle: Option<RenderTexture>,
    pub active_drop_target: RenderTexture,
    pub drop_target: RenderTexture,
}

impl Graphics {
    pub fn new(renderer: &mut Gles2Renderer) -> Self {
        let active_drop_target =
            Texture::from_buffer(renderer, &ACTIVE_DROP_TARGET_RGBA, 1, 1, false);
        let drop_target = Texture::from_buffer(renderer, &DROP_TARGET_RGBA, 1, 1, false);
        Self {
            active_drop_target: RenderTexture::new(active_drop_target),
            drop_target: RenderTexture::new(drop_target),
            gesture_handle: None,
        }
    }

    /// Get texture for the gesture handle.
    pub fn gesture_handle(
        &mut self,
        renderer: &mut Gles2Renderer,
        canvas: &Canvas,
    ) -> RenderTexture {
        // Initialize texture or replace it after scale change.
        let width = canvas.size().to_physical(canvas.scale()).w;
        let height = GESTURE_HANDLE_HEIGHT * canvas.scale();
        if self
            .gesture_handle
            .as_ref()
            .map_or(true, |handle| handle.size.w != width || handle.size.h != height)
        {
            // Initialize a white buffer with the correct size.
            let mut buffer = vec![255; (height * width * 4) as usize];

            // Calculate notch size.
            let notch_height = (height as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;
            let notch_width = (width as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;

            // Fill everything other than the handle's notch with black pixels.
            for x in 0..width {
                for y in 0..height {
                    if y < (height - notch_height) / 2
                        || y >= (height + notch_height) / 2
                        || x < (width - notch_width) / 2
                        || x >= (width + notch_width) / 2
                    {
                        let offset = (y * width + x) as usize * 4;
                        buffer[offset..offset + 3].copy_from_slice(&[0, 0, 0]);
                    }
                }
            }

            let texture = Texture::from_buffer(renderer, &buffer, width, height, true);
            self.gesture_handle = Some(RenderTexture(Rc::new(texture)));
        }

        // SAFETY: The code above ensures the `Option` is `Some`.
        unsafe { self.gesture_handle.clone().unwrap_unchecked() }
    }
}

/// Surface data store.
pub struct CatacombSurfaceData {
    pub opaque_region: Vec<(RectangleKind, Rectangle<i32, Logical>)>,
    pub texture: Option<RenderTexture>,
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
                let old_size = self.size;
                self.scale = attributes.buffer_scale;
                self.size = renderer::buffer_dimensions(&buffer)
                    .unwrap_or_default()
                    .to_logical(self.scale, self.transform);
                self.transform = attributes.buffer_transform.into();
                self.buffer = Some(Buffer::from(buffer));
                self.texture = None;

                // Reset damage on buffer resize.
                if old_size != self.size {
                    self.damage.tracker.reset();
                }
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
        self.damage.tracker.add([logical.to_physical(self.scale)]);
        self.damage.buffer.push(buffer);
    }
}

/// Pending buffer damage.
#[derive(Default)]
pub struct Damage {
    buffer: Vec<Rectangle<i32, BufferSpace>>,
    tracker: DamageTracker<i32, Physical>,
}

impl Damage {
    /// Get pending buffer damage.
    pub fn buffer(&self) -> &[Rectangle<i32, BufferSpace>] {
        self.buffer.as_slice()
    }

    /// Clear all damage.
    pub fn clear(&mut self) {
        self.buffer.clear();
    }
}
