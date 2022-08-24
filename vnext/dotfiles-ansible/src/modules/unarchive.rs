use crate::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Unarchive {
    #[serde(alias = "attr")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attributes: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub copy: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub decrypt: Option<bool>,

    pub dest: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub exclude: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub extra_opts: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub include: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub io_buffer_size: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub keep_newer: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub list_files: Option<bool>,

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

    pub src: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub unsafe_writes: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub validate_certs: Option<String>,
}
