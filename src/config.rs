//! Compositor configuration.

use catacomb_ipc::GestureSector;
use regex::{Error as RegexError, Regex};

/// Application used as application drawer.
pub const APP_DRAWER: &str = "tzompantli";

/// User-defined gesture action.
#[derive(Debug)]
pub struct GestureBinding {
    pub app_id: AppIdMatcher,
    pub start: GestureSector,
    pub end: GestureSector,
    pub program: String,
    pub arguments: Vec<String>,
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
