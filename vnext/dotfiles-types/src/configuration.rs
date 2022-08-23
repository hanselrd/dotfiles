use serde::{Deserialize, Serialize};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum Configuration {
    #[cfg_attr(target_os = "windows", default)]
    Desktop,
    #[cfg_attr(target_os = "macos", default)]
    Laptop,
    #[cfg_attr(target_os = "linux", default)]
    Server,
}
