//! Catacomb compositor interface.
//!
//! This library provides abstractions for interacting with Catacomb's external
//! interfaces from Wayland clients.

//! IPC socket communication.

use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
#[cfg(feature = "clap")]
use std::str::FromStr;
use std::{env, process};

#[cfg(feature = "clap")]
use clap::error::{Error as ClapError, ErrorKind as ClapErrorKind};
#[cfg(feature = "clap")]
use clap::{Subcommand, ValueEnum};
use regex::{Error as RegexError, Regex};
use serde::{Deserialize, Serialize};
#[cfg(feature = "smithay")]
use smithay::input::keyboard::ModifiersState;
#[cfg(feature = "smithay")]
use smithay::utils::{Logical, Point, Size, Transform};
#[cfg(feature = "clap")]
use xkbcommon::xkb;
#[cfg(feature = "clap")]
use xkbcommon::xkb::keysyms;

/// IPC message format.
#[cfg_attr(feature = "clap", derive(Subcommand))]
#[derive(Deserialize, Serialize, Debug)]
pub enum IpcMessage {
    /// Screen rotation (un)locking.
    Orientation {
        /// Lock rotation in the specified orientation.
        #[cfg_attr(feature = "clap", clap(long, num_args = 0..=1, conflicts_with = "unlock"))]
        lock: Option<Orientation>,
        /// Clear screen rotation lock.
        #[cfg_attr(feature = "clap", clap(long))]
        unlock: bool,
    },
    /// Output power management.
    Dpms {
        /// Desired power management state.
        state: DpmsState,
    },
    /// Update output scale factor.
    Scale {
        /// New scale factor.
        ///
        /// For the global scale this can be any float, which will be rounded
        /// internally.
        ///
        /// For window scales this can either be a float, or arithmetic on top
        /// of the global scale using either `+`, `-`, `*`, or `/` as prefix
        /// (i.e. `+0.5`).
        scale: WindowScale,
        /// App ID regex for per-window scaling.
        #[cfg_attr(feature = "clap", clap(long))]
        app_id: Option<String>,
    },
    /// Add a gesture.
    BindGesture {
        /// App ID regex.
        ///
        /// The binding will be enabled when the focused window's App ID matches
        /// the regex.
        ///
        /// Use `*` to bind the gesture globally.
        app_id: String,
        /// Starting sector of the gesture.
        start: GestureSector,
        /// Termination sector of the gesture.
        end: GestureSector,
        /// Program or keybinding this gesture should spawn.
        program: String,
        /// Arguments for this gesture's program.
        #[cfg_attr(feature = "clap", clap(allow_hyphen_values = true, trailing_var_arg = true))]
        arguments: Vec<String>,
    },
    /// Add a gesture keybinding.
    BindGestureKey {
        /// App ID regex.
        ///
        /// The binding will be enabled when the focused window's App ID matches
        /// the regex.
        ///
        /// Use `*` to bind the gesture globally.
        app_id: String,
        /// Starting sector of the gesture.
        start: GestureSector,
        /// Termination sector of the gesture.
        end: GestureSector,
        /// Desired modifiers.
        #[cfg_attr(feature = "clap", clap(long, short))]
        mods: Option<Modifiers>,
        /// X11 keysym for this binding.
        key: ClapKeysym,
    },
    /// Remove a gesture.
    UnbindGesture {
        /// App ID regex of the gesture.
        app_id: String,
        /// Starting sector of the gesture.
        start: GestureSector,
        /// Termination sector of the gesture.
        end: GestureSector,
    },
    /// Add a key.
    BindKey {
        /// App ID regex.
        ///
        /// The binding will be enabled when the focused window's App ID matches
        /// the regex.
        ///
        /// Use `*` to bind the key globally.
        app_id: String,
        /// Required modifiers.
        #[cfg_attr(feature = "clap", clap(long, short))]
        mods: Option<Modifiers>,
        /// Execute command on key press, rather than release.
        #[cfg_attr(feature = "clap", clap(long))]
        on_press: bool,
        /// Base key for this binding.
        key: ClapKeysym,
        /// Program this gesture should spawn.
        program: String,
        /// Arguments for this gesture's program.
        #[cfg_attr(feature = "clap", clap(allow_hyphen_values = true, trailing_var_arg = true))]
        arguments: Vec<String>,
    },
    /// Remove a gesture.
    UnbindKey {
        /// App ID regex of the gesture.
        app_id: String,
        /// Required modifiers.
        #[cfg_attr(feature = "clap", clap(long, short))]
        mods: Option<Modifiers>,
        /// Base key for this binding.
        key: ClapKeysym,
    },
}

/// Device orientation.
#[cfg_attr(feature = "clap", derive(ValueEnum))]
#[derive(Deserialize, Serialize, Default, PartialEq, Eq, Copy, Clone, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum Orientation {
    /// Portrait mode.
    #[default]
    Portrait,

    /// Inverse portrait mode.
    InversePortrait,

    /// Landscape mode.
    Landscape,

    /// Inverse landscape mode.
    InverseLandscape,
}

#[cfg(feature = "smithay")]
impl Orientation {
    /// Output rendering transform for this orientation.
    #[must_use]
    pub fn output_transform(&self) -> Transform {
        match self {
            Self::Portrait => Transform::Normal,
            Self::InversePortrait => Transform::_180,
            Self::Landscape => Transform::_90,
            Self::InverseLandscape => Transform::_270,
        }
    }

    /// Surface rendering transform for this orientation.
    #[must_use]
    pub fn surface_transform(&self) -> Transform {
        match self {
            Self::Portrait => Transform::Normal,
            Self::InversePortrait => Transform::_180,
            Self::Landscape => Transform::_270,
            Self::InverseLandscape => Transform::_90,
        }
    }
}

/// Output power state.
#[cfg_attr(feature = "clap", derive(ValueEnum))]
#[derive(Deserialize, Serialize, PartialEq, Eq, Copy, Clone, Debug)]
pub enum DpmsState {
    On,
    Off,
}

/// Gesture start/end sectors.
#[cfg_attr(feature = "clap", derive(ValueEnum))]
#[derive(Deserialize, Serialize, PartialEq, Eq, Copy, Clone, Debug)]
pub enum GestureSector {
    #[cfg_attr(feature = "clap", clap(alias = "tl"))]
    TopLeft,
    #[cfg_attr(feature = "clap", clap(alias = "tc"))]
    TopCenter,
    #[cfg_attr(feature = "clap", clap(alias = "tr"))]
    TopRight,
    #[cfg_attr(feature = "clap", clap(alias = "ml"))]
    MiddleLeft,
    #[cfg_attr(feature = "clap", clap(alias = "mc"))]
    MiddleCenter,
    #[cfg_attr(feature = "clap", clap(alias = "mr"))]
    MiddleRight,
    #[cfg_attr(feature = "clap", clap(alias = "bl"))]
    BottomLeft,
    #[cfg_attr(feature = "clap", clap(alias = "bc"))]
    BottomCenter,
    #[cfg_attr(feature = "clap", clap(alias = "br"))]
    BottomRight,
}

impl GestureSector {
    /// Get output sector a point lies in.
    #[cfg(feature = "smithay")]
    pub fn from_point(output_size: Size<f64, Logical>, point: Point<f64, Logical>) -> Self {
        // Map point in the range of 0..=2 for X and Y.
        let x_mult = (point.x / (output_size.w / 3.)).floor() as u32;
        let y_mult = (point.y / (output_size.h / 3.)).floor() as u32;

        match (x_mult.min(2), y_mult.min(2)) {
            (0, 0) => Self::TopLeft,
            (1, 0) => Self::TopCenter,
            (2, 0) => Self::TopRight,
            (0, 1) => Self::MiddleLeft,
            (1, 1) => Self::MiddleCenter,
            (2, 1) => Self::MiddleRight,
            (0, 2) => Self::BottomLeft,
            (1, 2) => Self::BottomCenter,
            (2, 2) => Self::BottomRight,
            _ => unreachable!(),
        }
    }
}

/// Window-specific scaling options.
#[derive(Deserialize, Serialize, Copy, Clone, PartialEq, Debug)]
pub enum WindowScale {
    Fixed(f64),
    Additive(f64),
    Subtractive(f64),
    Multiplicative(f64),
    Divisive(f64),
}

impl WindowScale {
    /// Calculate the scale relative to the provided output scale.
    pub fn scale(&self, output_scale: f64) -> f64 {
        // Get window scale based on current output scale.
        let mut scale = match self {
            Self::Fixed(scale) => *scale,
            Self::Additive(scale) => output_scale + scale,
            Self::Subtractive(scale) => output_scale - scale,
            Self::Multiplicative(scale) => output_scale * scale,
            Self::Divisive(scale) => output_scale / scale,
        };

        // Ensure scale factor is divisible by 120, to support wp_fractional_scale.
        scale = (scale * 120.).round() / 120.;

        scale
    }
}

#[cfg(feature = "clap")]
impl FromStr for WindowScale {
    type Err = ClapError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(ClapError::raw(ClapErrorKind::InvalidValue, "scale cannot be empty"));
        }

        // Get scale and variant.
        let (variant, factor): (fn(_) -> _, _) = match &s[..1] {
            "+" => (Self::Additive, &s[1..]),
            "-" => (Self::Subtractive, &s[1..]),
            "*" => (Self::Multiplicative, &s[1..]),
            "/" => (Self::Divisive, &s[1..]),
            _ => (Self::Fixed, s),
        };

        // Try to parse the scale.
        let num = f64::from_str(factor)
            .map_err(|err| ClapError::raw(ClapErrorKind::InvalidValue, err))?;

        Ok(variant(num))
    }
}

impl Display for WindowScale {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (prefix, scale) = match self {
            Self::Additive(scale) => ("+", scale),
            Self::Subtractive(scale) => ("-", scale),
            Self::Multiplicative(scale) => ("*", scale),
            Self::Divisive(scale) => ("/", scale),
            Self::Fixed(scale) => ("", scale),
        };
        write!(f, "{}{}", prefix, scale)
    }
}

/// User-defined App ID comparator.
#[derive(Debug)]
pub struct AppIdMatcher {
    variant: AppIdMatcherVariant,
    base: String,
}

impl AppIdMatcher {
    /// Check if this matcher captures the passed App ID.
    pub fn matches(&self, app_id: Option<&String>) -> bool {
        match (&self.variant, app_id) {
            (AppIdMatcherVariant::Global, _) => true,
            (AppIdMatcherVariant::Regex(regex), Some(app_id)) => regex.is_match(app_id),
            (AppIdMatcherVariant::Regex(_), None) => false,
        }
    }

    /// Get the raw matcher text.
    pub fn base(&self) -> &str {
        &self.base
    }
}

impl TryFrom<String> for AppIdMatcher {
    type Error = RegexError;

    fn try_from(base: String) -> Result<Self, Self::Error> {
        let variant = if base == "*" {
            AppIdMatcherVariant::Global
        } else {
            AppIdMatcherVariant::Regex(Regex::new(&base)?)
        };

        Ok(Self { base, variant })
    }
}

/// Variants for the App ID matcher.
#[derive(Debug)]
pub enum AppIdMatcherVariant {
    Regex(Regex),
    Global,
}

/// Modifier state for a key press.
#[derive(Deserialize, Serialize, PartialEq, Eq, Default, Copy, Clone, Debug)]
pub struct Modifiers {
    pub control: bool,
    pub shift: bool,
    pub logo: bool,
    pub alt: bool,
}

#[cfg(feature = "smithay")]
impl From<&ModifiersState> for Modifiers {
    fn from(mods: &ModifiersState) -> Self {
        Self { control: mods.ctrl, shift: mods.shift, logo: mods.logo, alt: mods.alt }
    }
}

#[cfg(feature = "clap")]
impl FromStr for Modifiers {
    type Err = ClapError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut modifiers = Self::default();

        let mods = s.split(',');
        for modifier in mods {
            match modifier.trim().to_lowercase().as_str() {
                "control" | "ctrl" => modifiers.control = true,
                "super" | "logo" => modifiers.logo = true,
                "shift" => modifiers.shift = true,
                "alt" => modifiers.alt = true,
                invalid => {
                    return Err(ClapError::raw(
                        ClapErrorKind::InvalidValue,
                        format!(
                            "invalid modifier {invalid:?}, expected one of \"shift\", \
                             \"control\", \"alt\", or \"super\""
                        ),
                    ))
                },
            }
        }

        Ok(modifiers)
    }
}

/// Clap wrapper for XKB keysym.
#[derive(Deserialize, Serialize, Copy, Clone, Debug)]
pub struct ClapKeysym(pub u32);

#[cfg(feature = "clap")]
impl FromStr for ClapKeysym {
    type Err = ClapError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match xkb::keysym_from_name(s, xkb::KEYSYM_NO_FLAGS).raw() {
            keysyms::KEY_NoSymbol => {
                Err(ClapError::raw(ClapErrorKind::InvalidValue, format!("invalid keysym {s:?}")))
            },
            keysym => Ok(Self(keysym)),
        }
    }
}

/// Send a message to the Catacomb IPC socket.
pub fn send_message(message: &IpcMessage) -> Result<(), Box<dyn Error>> {
    // Ensure IPC message is legal.
    validate_message(message)?;

    let socket_name = match env::var("WAYLAND_DISPLAY") {
        Ok(socket_name) => socket_name,
        Err(_) => {
            eprintln!("Error: WAYLAND_DISPLAY is not set");
            process::exit(101);
        },
    };

    let socket_path = socket_path(&socket_name);

    // Ensure Catacomb's IPC listener is running.
    if !socket_path.exists() {
        eprintln!("Error: IPC socket not found, ensure Catacomb is running");
        process::exit(102);
    }

    let mut socket = UnixStream::connect(&socket_path)?;

    let message = serde_json::to_string(&message)?;
    socket.write_all(message[..].as_bytes())?;
    socket.flush()?;

    Ok(())
}

/// Path for the IPC socket file.
pub fn socket_path(socket_name: &str) -> PathBuf {
    dirs::runtime_dir().unwrap_or_else(env::temp_dir).join(format!("catacomb-{socket_name}.sock"))
}

/// Validate a message beyond simple clap parsing.
fn validate_message(message: &IpcMessage) -> Result<(), Box<dyn Error>> {
    match message {
        // Ensure App IDs are valid regexes.
        IpcMessage::Scale { app_id: Some(app_id), .. }
        | IpcMessage::BindGesture { app_id, .. }
        | IpcMessage::BindKey { app_id, .. } => {
            AppIdMatcher::try_from(app_id.clone())?;
        },
        // Ensure only fixed scales are used for global scale changes.
        IpcMessage::Scale { scale, app_id: None } if !matches!(scale, WindowScale::Fixed(_)) => {
            return Err(format!("global scale must be fixed, got \"{scale}\"").into());
        },
        _ => (),
    }

    Ok(())
}
