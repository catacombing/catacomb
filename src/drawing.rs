//! Drawing utilities.

use std::cell::Cell;
use std::ops::Deref;
use std::rc::Rc;

use catacomb_ipc::WindowScale;
use smithay::backend::renderer::element::utils::{
    CropRenderElement, Relocate, RelocateRenderElement, RescaleRenderElement,
};
use smithay::backend::renderer::element::{Element, Id, RenderElement, UnderlyingStorage};
use smithay::backend::renderer::gles::{ffi, GlesFrame, GlesRenderer, GlesTexture};
use smithay::backend::renderer::utils::{
    Buffer, CommitCounter, DamageBag, DamageSet, DamageSnapshot,
};
use smithay::backend::renderer::{self, Renderer, Texture as _};
use smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer;
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
use crate::protocols::single_pixel_buffer;

/// Color of the hovered overview tiling location highlight.
const ACTIVE_DROP_TARGET_RGBA: [u8; 4] = [64, 64, 64, 128];

/// Color of the overview tiling location highlight.
const URGENCY_ICON_RGBA: [u8; 4] = [172, 66, 66, 255];

/// Color of the overview tiling location highlight.
const DROP_TARGET_RGBA: [u8; 4] = [32, 32, 32, 64];

/// Relative size of gesture notch to the handle's whole width/height.
const GESTURE_NOTCH_PERCENTAGE: f64 = 0.2;

/// Gesture handle color with automatic IME control.
const GESTURE_HANDLE_DEFAULT_RGBA: [u8; 3] = [255; 3];

/// Gesture handle color with IME force-enabled.
const GESTURE_HANDLE_LOCKED_RGBA: [u8; 3] = [42, 117, 42];

/// Gesture handle color with IME force-disabled.
const GESTURE_HANDLE_BLOCKED_RGBA: [u8; 3] = [117, 42, 42];

/// Cached texture.
///
/// Includes all information necessary to render a surface's texture even after
/// the surface itself has already died.
#[derive(Clone, Debug)]
pub struct Texture {
    opaque_regions: Vec<Rectangle<i32, Logical>>,
    tracker: DamageSnapshot<i32, Logical>,
    location: Cell<Point<i32, Logical>>,
    src_rect: Rectangle<f64, Logical>,
    dst_size: Size<i32, Logical>,
    buffer_size: Size<i32, Logical>,
    buffer: Option<Buffer>,
    texture: GlesTexture,
    transform: Transform,
    window_scale: Option<WindowScale>,
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
        let opaque_regions = if opaque { vec![src_rect] } else { Vec::new() };

        Self {
            opaque_regions,
            buffer_size,
            texture,
            scale,
            tracker: DamageSnapshot::empty(),
            src_rect: src_rect.to_f64(),
            dst_size: buffer_size,
            id: Id::new(),
            window_scale: Default::default(),
            transform: Default::default(),
            location: Default::default(),
            buffer: Default::default(),
        }
    }

    /// Create a texture from a Wayland surface.
    pub fn from_surface(
        texture: GlesTexture,
        window_scale: Option<WindowScale>,
        location: impl Into<Point<i32, Logical>>,
        buffer: &CatacombSurfaceData,
        surface: &WlSurface,
    ) -> Self {
        let location = location.into();

        // Get surface's opaque region.
        let mut opaque_regions = Vec::new();
        for (kind, rect) in &buffer.opaque_region {
            match kind {
                RectangleKind::Add => opaque_regions.push(*rect),
                RectangleKind::Subtract => opaque_regions.subtract_rect(*rect),
            }
        }

        Self {
            opaque_regions,
            window_scale,
            texture,
            tracker: buffer.damage.tracker.snapshot(),
            buffer_size: buffer.buffer_size,
            location: Cell::new(location),
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
        let texture = create_texture(renderer, buffer, width, height, opaque);
        let logical_size =
            Size::<i32, Physical>::from((width, height)).to_f64().to_logical(scale).to_i32_round();
        Texture::new(texture, logical_size, scale, opaque)
    }

    /// Move texture to a different location.
    pub fn set_location(&self, location: Point<i32, Logical>) {
        self.location.replace(location);
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
        let scale = self.window_scale.map_or(scale.x, |window_scale| window_scale.scale(scale.x));
        Rectangle::from_loc_and_size(self.location.get(), self.dst_size)
            .to_physical_precise_round(scale)
    }

    fn damage_since(
        &self,
        scale: Scale<f64>,
        commit: Option<CommitCounter>,
    ) -> DamageSet<i32, Physical> {
        let damage = match self.tracker.damage_since(commit) {
            Some(damage) => damage,
            // Fallback to fully damage.
            None => {
                let size = self.geometry(scale).size;
                return DamageSet::from_slice(&[Rectangle::from_loc_and_size((0, 0), size)]);
            },
        };

        // Transform output to window-specific scale.
        let scale = self.window_scale.map_or(scale.x, |window_scale| window_scale.scale(scale.x));

        // Convert damage to physical space.
        DamageSet::from_iter(damage.into_iter().map(|rect| rect.to_physical_precise_round(scale)))
    }

    fn opaque_regions(&self, scale: Scale<f64>) -> Vec<Rectangle<i32, Physical>> {
        let scale = self.window_scale.map_or(scale.x, |window_scale| window_scale.scale(scale.x));
        self.opaque_regions.iter().map(|rect| rect.to_physical_precise_round(scale)).collect()
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
        bounds: Rectangle<i32, Physical>,
        window_scale: impl Into<Option<f64>>,
        output_scale: f64,
    ) {
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
    ) -> DamageSet<i32, Physical> {
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
    pub active_drop_target: RenderTexture,
    pub urgency_icon: RenderTexture,
    pub drop_target: RenderTexture,

    gesture_handle_default: Option<RenderTexture>,
    gesture_handle_blocked: Option<RenderTexture>,
    gesture_handle_locked: Option<RenderTexture>,
}

impl Graphics {
    pub fn new(renderer: &mut GlesRenderer) -> Self {
        let active_drop_target =
            Texture::from_buffer(renderer, 1., &ACTIVE_DROP_TARGET_RGBA, 1, 1, false);
        let drop_target = Texture::from_buffer(renderer, 1., &DROP_TARGET_RGBA, 1, 1, false);

        let urgency_icon = Texture::from_buffer(renderer, 1., &URGENCY_ICON_RGBA, 1, 1, true);

        Self {
            active_drop_target: RenderTexture::new(active_drop_target),
            urgency_icon: RenderTexture::new(urgency_icon),
            drop_target: RenderTexture::new(drop_target),
            gesture_handle_default: None,
            gesture_handle_blocked: None,
            gesture_handle_locked: None,
        }
    }

    /// Get texture for the gesture handle.
    pub fn gesture_handle(
        &mut self,
        renderer: &mut GlesRenderer,
        canvas: &Canvas,
        ime_override: Option<bool>,
    ) -> RenderTexture {
        let (handle, color) = match ime_override {
            None => (&mut self.gesture_handle_default, GESTURE_HANDLE_DEFAULT_RGBA),
            Some(true) => (&mut self.gesture_handle_locked, GESTURE_HANDLE_LOCKED_RGBA),
            Some(false) => (&mut self.gesture_handle_blocked, GESTURE_HANDLE_BLOCKED_RGBA),
        };

        // Initialize texture or replace it after scale change.
        let scale = canvas.scale();
        let width = canvas.physical_size().w;
        let height = (GESTURE_HANDLE_HEIGHT as f64 * scale).round() as i32;
        if handle.as_ref().map_or(true, |handle| {
            handle.texture.width() != width as u32 || handle.texture.height() != height as u32
        }) {
            // Initialize a black buffer with the correct size.
            let mut buffer = vec![0; (height * width * 4) as usize];

            // Calculate notch size.
            let notch_height = (height as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;
            let notch_width = (width as f64 * GESTURE_NOTCH_PERCENTAGE) as i32;

            // Fill handle's notch with the correct color.
            for x in 0..width {
                for y in 0..height {
                    if y >= (height - notch_height) / 2
                        && y < (height + notch_height) / 2
                        && x >= (width - notch_width) / 2
                        && x < (width + notch_width) / 2
                    {
                        let offset = (y * width + x) as usize * 4;
                        buffer[offset..offset + 3].copy_from_slice(&color);
                    }
                }
            }

            let texture = Texture::from_buffer(renderer, scale, &buffer, width, height, true);
            *handle = Some(RenderTexture(Rc::new(texture)));
        }

        // SAFETY: The code above ensures the `Option` is `Some`.
        unsafe { handle.clone().unwrap_unchecked() }
    }
}

/// Surface data store.
pub struct CatacombSurfaceData {
    pub opaque_region: Vec<(RectangleKind, Rectangle<i32, Logical>)>,
    pub preferred_fractional_scale: f64,
    pub texture: Option<RenderTexture>,
    pub buffer_size: Size<i32, Logical>,
    pub src_rect: Rectangle<f64, Logical>,
    pub dst_size: Size<i32, Logical>,
    pub inhibits_shortcuts: bool,
    pub buffer: Option<Buffer>,
    pub transform: Transform,
    pub damage: Damage,
    pub scale: i32,

    // Location cache to avoid recalculation when iterating surfaces.
    pub location: Point<i32, Logical>,
}

impl Default for CatacombSurfaceData {
    fn default() -> Self {
        Self {
            preferred_fractional_scale: 1.,
            scale: 1,
            inhibits_shortcuts: Default::default(),
            opaque_region: Default::default(),
            buffer_size: Default::default(),
            transform: Default::default(),
            src_rect: Default::default(),
            dst_size: Default::default(),
            location: Default::default(),
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
                self.buffer_size = buffer_dimensions(&buffer, self.scale, self.transform);
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

/// Get WlBuffer dimensions.
///
/// NOTE: This can be removed once the single-pixel buffer protocol is supported
/// upstream.
fn buffer_dimensions(buffer: &WlBuffer, scale: i32, transform: Transform) -> Size<i32, Logical> {
    match single_pixel_buffer::get_single_pixel_buffer(buffer) {
        Ok(_) => Size::from((1, 1)),
        Err(_) => renderer::buffer_dimensions(buffer).unwrap_or_default(),
    }
    .to_logical(scale, transform)
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

/// Create a new OpenGL texture.
pub fn create_texture(
    renderer: &mut GlesRenderer,
    buffer: &[u8],
    width: i32,
    height: i32,
    opaque: bool,
) -> GlesTexture {
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
    unsafe { GlesTexture::from_raw(renderer, Some(format), opaque, texture_id, size) }
}
