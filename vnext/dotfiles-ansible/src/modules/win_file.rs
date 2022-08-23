use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    Directory,
    #[default]
    File,
    Touch,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinFile {
    #[serde(alias = "dest", alias = "name")]
    pub path: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,
}
