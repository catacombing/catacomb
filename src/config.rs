//! Compositor configuration.

use catacomb_ipc::{AppIdMatcher, GestureSector};

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
