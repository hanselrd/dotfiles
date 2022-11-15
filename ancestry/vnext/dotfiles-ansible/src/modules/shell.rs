use serde::Serialize;
use std::path::PathBuf;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Shell {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub chdir: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub cmd: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub executable: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub removes: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdin: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdin_add_newline: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub warn: Option<bool>,
}
