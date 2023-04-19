//! Compositor configuration.

use catacomb_ipc::GestureSector;

/// Application used as application drawer.
pub const APP_DRAWER: &str = "tzompantli";

/// User-defined gesture action.
#[derive(Debug)]
pub struct GestureBinding {
    pub app_id: String,
    pub start: GestureSector,
    pub end: GestureSector,
    pub program: String,
    pub arguments: Vec<String>,
}
