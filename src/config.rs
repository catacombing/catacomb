//! Compositor configuration.

use catacomb_ipc::{AppIdMatcher, GestureSector, KeyTrigger, Keysyms, Modifiers};

/// User-defined gesture action.
#[derive(Debug)]
pub struct GestureBinding {
    pub app_id: AppIdMatcher,
    pub start: GestureSector,
    pub end: GestureSector,
    pub action: GestureBindingAction,
}

/// Action variants for gesture bindings.
#[derive(Clone, Debug)]
pub enum GestureBindingAction {
    Cmd((String, Vec<String>)),
    Key((u32, Modifiers)),
}

/// User-defined key action.
#[derive(Debug)]
pub struct KeyBinding {
    pub app_id: AppIdMatcher,
    pub mods: Modifiers,
    pub keys: Keysyms,
    pub program: String,
    pub arguments: Vec<String>,
    pub trigger: KeyTrigger,
}
