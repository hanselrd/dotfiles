use clap::ValueEnum;
use serde::{Deserialize, Serialize};

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    ValueEnum,
)]
#[serde(rename_all = "lowercase")]
pub enum Preset {
    #[cfg_attr(any(target_os = "windows", target_os = "macos"), default)]
    Full,
    #[cfg_attr(target_os = "linux", default)]
    Minimal,
    Server,
}
