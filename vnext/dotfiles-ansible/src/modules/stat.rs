use serde::Serialize;
use std::path::PathBuf;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ChecksumAlgorithm {
    Md5,
    #[default]
    Sha1,
    Sha224,
    Sha256,
    Sha384,
    Sha512,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Stat {
    #[serde(alias = "checksum", alias = "checksum_algo")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum_algorithm: Option<ChecksumAlgorithm>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub follow: Option<bool>,

    #[serde(alias = "attr", alias = "attributes")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub get_attributes: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub get_checksum: Option<bool>,

    #[serde(alias = "mime", alias = "mime_type", alias = "mime-type")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub get_mime: Option<bool>,

    #[serde(alias = "dest", alias = "name")]
    pub path: PathBuf,
}
