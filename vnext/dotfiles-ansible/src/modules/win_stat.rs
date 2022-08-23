use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ChecksumAlgorithm {
    Md5,
    #[default]
    Sha1,
    Sha256,
    Sha384,
    Sha512,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinStat {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum_algorithm: Option<ChecksumAlgorithm>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub follow: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub get_checksum: Option<bool>,

    #[serde(alias = "dest", alias = "name")]
    pub path: String,
}
