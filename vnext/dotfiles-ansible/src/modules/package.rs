use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    #[default]
    Present,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Package {
    pub name: String,

    pub state: State,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#use: Option<String>,
}
