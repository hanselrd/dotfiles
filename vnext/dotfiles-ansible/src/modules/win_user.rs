use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GroupsAction {
    Add,
    #[default]
    Replace,
    Remove,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    #[default]
    Present,
    Query,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum UpdatePassword {
    #[default]
    Always,
    OnCreate,
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Serialize)]
pub struct WinUser {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_disabled: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub account_locked: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub fullname: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub groups: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub groups_action: Option<GroupsAction>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub home_directory: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub login_script: Option<String>,

    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password_expired: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password_never_expires: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub update_password: Option<UpdatePassword>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_cannot_change_password: Option<bool>,
}
