use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinUnzip {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<String>,

    #[serde(alias = "rm")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub delete_archive: Option<bool>,

    pub dest: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub recurse: Option<bool>,

    pub src: String,
}
