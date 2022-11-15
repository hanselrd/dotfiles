use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct IncludeTasks {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub apply: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,
}
