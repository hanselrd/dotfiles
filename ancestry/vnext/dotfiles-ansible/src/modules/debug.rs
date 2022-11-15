use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Debug {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub msg: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub var: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub verbosity: Option<u64>,
}
