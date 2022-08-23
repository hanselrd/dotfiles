use serde::{Deserialize, Serialize};

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum Privilege {
    #[cfg_attr(target_os = "linux", serde(rename = "root"))]
    Administrator,
    #[default]
    User,
}
