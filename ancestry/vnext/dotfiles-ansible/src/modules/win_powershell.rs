use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ErrorAction {
    SilentlyContinue,
    #[default]
    Continue,
    Stop,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct WinPowershell {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub chdir: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub depth: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_action: Option<ErrorAction>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub executable: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<HashMap<String, String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub removes: Option<String>,

    pub script: String,
}
