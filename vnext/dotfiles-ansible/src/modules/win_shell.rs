use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinShell {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub chdir: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub cmd: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub executable: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub removes: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdin: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdin_add_newline: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub warn: Option<bool>,
}
