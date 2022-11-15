use serde::{Deserialize, Serialize};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum System {
    #[cfg_attr(target_os = "windows", default)]
    Windows,
    #[cfg_attr(target_os = "macos", default)]
    MacOS,
    #[cfg_attr(target_os = "linux", default)]
    Linux,
}
