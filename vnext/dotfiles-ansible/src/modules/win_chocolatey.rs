use crate::types::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Architecture {
    #[default]
    Default,
    X86,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
Absent,
Downgrade,
Upgrade,
Latest,
    #[default]
Present,
Reinstalled,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct WinChocolatey {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_empty_checksums: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_multiple: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_prerelease: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub architecture: Option<Architecture>,

    #[serde(alias="install_ps1", alias="bootstrap_ps1")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bootstrap_script: Option<String>,

    #[serde(alias="licensed_args")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub choco_args: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ignore_checksums: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ignore_dependencies: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_args: Option<String>,

    pub name: Vec<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub override_args: Option<bool>,

    #[serde(alias="params")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package_params: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub pinned: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_url: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_username: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub remove_dependencies: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub skip_scripts: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_username: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,

    #[serde(alias="execution_timeout")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub validate_certs: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
}
