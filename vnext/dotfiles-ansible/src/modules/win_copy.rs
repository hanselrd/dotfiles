use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinCopy {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backup: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub decrypt: Option<bool>,

    pub dest: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub local_follow: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_src: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub src: Option<String>,
}
