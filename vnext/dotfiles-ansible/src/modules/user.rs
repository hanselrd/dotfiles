use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    #[default]
    Present,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum UpdatePassword {
    #[default]
    Always,
    OnCreate,
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Serialize)]
pub struct User {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub append: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub authorization: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,

    #[serde(alias = "createhome")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub create_home: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires: Option<f64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub generate_ssh_key: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub groups: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub hidden: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub home: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub local: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub login_class: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub move_home: Option<bool>,

    #[serde(alias = "user")]
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub non_unique: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password_expire_max: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password_expire_min: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub password_lock: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub remove: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub role: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub seuser: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub shell: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub skeleton: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssh_key_bits: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssh_key_comment: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssh_key_file: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssh_key_passphrase: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssh_key_type: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub uid: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub umask: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub update_password: Option<UpdatePassword>,
}
