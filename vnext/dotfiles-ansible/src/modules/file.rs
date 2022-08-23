use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    Directory,
    #[default]
    File,
    Hard,
    Link,
    Touch,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct File {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub access_time: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub access_time_format: Option<String>,

    #[serde(alias = "attr")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attributes: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub follow: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<file_mode::FileMode>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub modification_time: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub modification_time_format: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,

    #[serde(alias = "dest", alias = "name")]
    pub path: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub recurse: Option<bool>,

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
    pub state: Option<State>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub unsafe_writes: Option<bool>,
}
