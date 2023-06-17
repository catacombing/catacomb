//! Compositor configuration.

use catacomb_ipc::{AppIdMatcher, GestureSector, Modifiers};
use smithay::input::keyboard::Keysym;

/// User-defined gesture action.
#[derive(Debug)]
pub struct GestureBinding {
    pub app_id: AppIdMatcher,
    pub start: GestureSector,
    pub end: GestureSector,
    pub program: String,
    pub arguments: Vec<String>,
}

/// User-defined key action.
#[derive(Debug)]
pub struct KeyBinding {
    pub app_id: AppIdMatcher,
    pub mods: Modifiers,
    pub key: Keysym,
    pub program: String,
    pub arguments: Vec<String>,
}
