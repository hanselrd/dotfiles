use crate::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Copy {
    #[serde(alias = "attr")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attributes: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub backup: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub decrypt: Option<bool>,

    pub dest: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory_mode: Option<types::FileMode>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub follow: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub local_follow: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<types::FileMode>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_src: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub selevel: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub serole: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub setype: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub seuser: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub src: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub unsafe_writes: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub validate: Option<String>,
}
