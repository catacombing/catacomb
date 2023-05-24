//! Drawing utilities.

use std::ops::Deref;
use std::rc::Rc;

use smithay::backend::renderer::element::utils::{
    CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement,
};
use smithay::backend::renderer::element::{Element, Id, RenderElement, UnderlyingStorage};
use smithay::backend::renderer::gles::{ffi, GlesFrame, GlesRenderer, GlesTexture};
use smithay::backend::renderer::utils::{Buffer, CommitCounter, DamageBag, DamageSnapshot};
use smithay::backend::renderer::{self, Renderer};
use smithay::reexports::wayland_server::protocol::wl_surface::WlSurface;
use smithay::utils::{
    Buffer as BufferSpace, Logical, Physical, Point, Rectangle, Scale, Size, Transform,
};
use smithay::wayland::compositor::{
    BufferAssignment, Damage as SurfaceDamage, RectangleKind, SurfaceAttributes, SurfaceData,
};
use smithay::wayland::viewporter::{self, ViewportCachedState};

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
    tracker: DamageSnapshot<i32, Logical>,
    location: Point<i32, Logical>,
    src_rect: Rectangle<f64, Logical>,
    dst_size: Size<i32, Logical>,
    buffer_size: Size<i32, Logical>,
    buffer: Option<Buffer>,
    texture: GlesTexture,
    transform: Transform,
    scale: f64,
    id: Id,
}

impl Texture {
    /// Create a texture from an OpenGL texture.
    pub fn new(
        texture: GlesTexture,
        buffer_size: impl Into<Size<i32, Logical>>,
        scale: f64,
        opaque: bool,
    ) -> Self {
        let buffer_size = buffer_size.into();
        let src_rect = Rectangle::from_loc_and_size((0, 0), buffer_size);

        // Ensure fully opaque textures are treated as such.
        let opaque_regions = if opaque { vec![src_rect.to_physical(1)] } else { Vec::new() };

        Self {
            opaque_regions,
            buffer_size,
            texture,
            scale,
            tracker: DamageSnapshot::empty(),
            src_rect: src_rect.to_f64(),
            dst_size: buffer_size,
            id: Id::new(),
            transform: Default::default(),
            location: Default::default(),
            buffer: Default::default(),
        }
    }

    /// Create a texture from a Wayland surface.
    pub fn from_surface(
        texture: GlesTexture,
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
            buffer_size: buffer.buffer_size,
            buffer: buffer.buffer.clone(),
            transform: buffer.transform,
            scale: buffer.scale as f64,
            src_rect: buffer.src_rect,
            dst_size: buffer.dst_size,
            id: surface.into(),
        }
    }

    /// Create a texture from an RGBA buffer.
    pub fn from_buffer(
        renderer: &mut GlesRenderer,
        scale: f64,
        buffer: &[u8],
        width: i32,
        height: i32,
        opaque: bool,
    ) -> Self {
        assert!(buffer.len() as i32 >= width * height * 4);

        let format = ffi::RGBA;
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
                    format as i32,
                    width,
                    height,
                    0,
                    format,
                    ffi::UNSIGNED_BYTE,
                    buffer.as_ptr().cast(),
                );
                gl.BindTexture(ffi::TEXTURE_2D, 0);

                tex
            })
            .expect("create texture");

        let size = (width, height).into();
        let texture =
            unsafe { GlesTexture::from_raw(renderer, Some(format), opaque, texture_id, size) };

        Texture::new(texture, (width, height), scale, opaque)
    }

    /// Target size after rendering.
    pub fn size(&self) -> Size<i32, Logical> {
        self.dst_size
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
        self.src_rect.to_buffer(self.scale, self.transform, &self.buffer_size.to_f64())
    }

    fn geometry(&self, scale: Scale<f64>) -> Rectangle<i32, Physical> {
        Rectangle::from_loc_and_size(self.location, self.dst_size).to_physical_precise_round(scale)
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> Vec<Rectangle<i32, Physical>> {
        let mut damage = match self.tracker.damage_since(commit) {
            Some(damage) => damage,
            // Fallback to fully damage.
            None => return vec![Rectangle::from_loc_and_size((0, 0), self.geometry(scale).size)],
        };

        // Apply viewporter transforms to damage.
        let viewporter_scale = self.dst_size.to_f64() / self.src_rect.size;
        let damage = damage.drain(..).flat_map(|damage| {
            // Limit damage to element's source rect.
            let mut damage = damage.to_f64().intersection(self.src_rect)?;

            // Scale source damage to dst space.
            damage = damage.upscale(viewporter_scale);

            // Convert damage to physical coordinates.
            Some(damage.to_physical_precise_up(scale))
        });
        damage.collect()
    }

    fn opaque_regions(&self, _scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        self.opaque_regions.clone()
    }
}

impl RenderElement<GlesRenderer> for RenderTexture {
    fn draw<'a>(
        &self,
        frame: &mut GlesFrame,
        src: Rectangle<f64, BufferSpace>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlesRenderer as Renderer>::Error> {
        frame.render_texture_from_to(&self.texture, src, dst, damage, self.transform, 1., None, &[])
    }

    fn underlying_storage(&self, _renderer: &mut GlesRenderer) -> Option<UnderlyingStorage> {
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
        output_scale: f64,
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
        let cropped_element =
            CropRenderElement::from_element(relocated_element, output_scale, bounds);

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

impl RenderElement<GlesRenderer> for CatacombElement {
    fn draw<'a>(
        &self,
        frame: &mut GlesFrame,
        src: Rectangle<f64, BufferSpace>,
        dst: Rectangle<i32, Physical>,
        damage: &[Rectangle<i32, Physical>],
    ) -> Result<(), <GlesRenderer as Renderer>::Error> {
        self.0.draw(frame, src, dst, damage)
    }

    fn underlying_storage(&self, renderer: &mut GlesRenderer) -> Option<UnderlyingStorage> {
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
    pub fn new(renderer: &mut GlesRenderer) -> Self {
        let active_drop_target =
            Texture::from_buffer(renderer, 1., &ACTIVE_DROP_TARGET_RGBA, 1, 1, false);
        let drop_target = Texture::from_buffer(renderer, 1., &DROP_TARGET_RGBA, 1, 1, false);
        Self {
            active_drop_target: RenderTexture::new(active_drop_target),
            drop_target: RenderTexture::new(drop_target),
            gesture_handle: None,
        }
    }

    /// Get texture for the gesture handle.
    pub fn gesture_handle(
        &mut self,
        renderer: &mut GlesRenderer,
        canvas: &Canvas,
    ) -> RenderTexture {
        // Initialize texture or replace it after scale change.
        let scale = canvas.scale();
        let width = canvas.physical_size().w;
        let height = (GESTURE_HANDLE_HEIGHT as f64 * scale).round() as i32;
        if self
            .gesture_handle
            .as_ref()
            .map_or(true, |handle| handle.size().w != width || handle.size().h != height)
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

            let texture = Texture::from_buffer(renderer, scale, &buffer, width, height, true);
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
    pub buffer_size: Size<i32, Logical>,
    pub src_rect: Rectangle<f64, Logical>,
    pub dst_size: Size<i32, Logical>,
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
            buffer_size: Default::default(),
            transform: Default::default(),
            src_rect: Default::default(),
            dst_size: Default::default(),
            texture: Default::default(),
            buffer: Default::default(),
            damage: Default::default(),
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
        surface_data: &SurfaceData,
        attributes: &mut SurfaceAttributes,
        assignment: BufferAssignment,
    ) {
        match assignment {
            BufferAssignment::NewBuffer(buffer) => {
                let old_size = self.buffer_size;
                self.scale = attributes.buffer_scale;
                self.transform = attributes.buffer_transform.into();
                self.buffer_size = renderer::buffer_dimensions(&buffer)
                    .unwrap_or_default()
                    .to_logical(self.scale, self.transform);
                self.buffer = Some(Buffer::from(buffer));
                self.texture = None;

                // Check for viewporter src/dst.
                let viewport_valid =
                    viewporter::ensure_viewport_valid(surface_data, self.buffer_size);
                let (viewport_src, viewport_dst) = if viewport_valid {
                    let viewport = surface_data.cached_state.current::<ViewportCachedState>();
                    (viewport.src, viewport.size())
                } else {
                    (None, None)
                };

                // Fallback to buffer size without viewporter.
                self.src_rect = viewport_src.unwrap_or_else(|| {
                    Rectangle::from_loc_and_size((0.0, 0.0), self.buffer_size.to_f64())
                });
                self.dst_size = viewport_dst.unwrap_or(self.buffer_size);

                // Reset damage on buffer resize.
                if old_size != self.buffer_size {
                    self.damage.tracker.reset();
                }
            },
            BufferAssignment::Removed => *self = Self::default(),
        }

        // Store new damage updates.
        let physical_damage = attributes.damage.drain(..).map(|damage| {
            // Get damage in buffer and logical space.
            let (buffer, logical) = match damage {
                SurfaceDamage::Buffer(buffer) => {
                    let buffer_size = self.buffer_size.to_buffer(self.scale, self.transform);
                    let logical = buffer.to_logical(self.scale, self.transform, &buffer_size);
                    (buffer, logical)
                },
                SurfaceDamage::Surface(logical) => {
                    let buffer = logical.to_buffer(self.scale, self.transform, &self.buffer_size);
                    (buffer, logical)
                },
            };

            // Store buffer space damage for buffer import.
            self.damage.buffer.push(buffer);

            logical
        });
        self.damage.tracker.add(physical_damage);
    }
}

/// Pending buffer damage.
#[derive(Default)]
pub struct Damage {
    buffer: Vec<Rectangle<i32, BufferSpace>>,
    tracker: DamageBag<i32, Logical>,
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
