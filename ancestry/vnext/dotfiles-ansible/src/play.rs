use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct Play {
    pub hosts: Vec<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub gather_facts: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#become: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub roles: Option<Vec<String>>,
}
