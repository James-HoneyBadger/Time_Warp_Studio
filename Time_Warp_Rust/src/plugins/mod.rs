// Plugin system module - to be implemented in Phase 8
#[cfg(feature = "plugins")]
pub struct PluginManager;

#[cfg(feature = "plugins")]
impl PluginManager {
    pub fn new() -> Self {
        Self
    }
}
