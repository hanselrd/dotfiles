use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Ping {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<String>,
}
