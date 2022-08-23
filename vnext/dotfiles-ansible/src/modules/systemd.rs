use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Scope {
    #[default]
    System,
    User,
    Global,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Reloaded,
    Restarted,
    #[default]
    Started,
    Stopped,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Systemd {
    #[serde(alias = "daemon-reexec")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub daemon_reexec: Option<bool>,

    #[serde(alias = "daemon-reload")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub daemon_reload: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub masked: Option<bool>,

    #[serde(alias = "service", alias = "unit")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub no_block: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub scope: Option<Scope>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,
}
