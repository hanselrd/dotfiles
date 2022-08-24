use serde::Serialize;
use std::path::PathBuf;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinShell {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub chdir: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub executable: Option<PathBuf>,

    #[rustfmt::skip]
    // This task is somewhat broken because it doesn't accept a cmd like
    // the normal shell module
    // pub free_form: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub no_profile: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_encoding_override: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub removes: Option<PathBuf>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdin: Option<String>,
}
