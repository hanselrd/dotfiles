use crate::types::*;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FollowRedirects {
    All,
    None,
    #[default]
    Safe,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Provider {
    #[default]
    Auto,
    Msi,
    Msix,
    Msp,
    Registry,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    Absent,
    #[default]
    Present,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct WinPackage {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub chdir: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_cert: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_cert_password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates_path: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates_service: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub creates_version: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected_return_code: Option<Vec<u64>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub follow_redirects: Option<FollowRedirects>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub force_basic_auth: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub http_agent: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub log_path: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub maximum_redirection: Option<u64>,

    #[serde(alias = "user_password")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,

    #[serde(alias = "productid")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub product_id: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<Provider>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_url: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_use_default_credential: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_username: Option<String>,

    #[serde(alias = "ensure")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<State>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub url_method: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub url_password: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub url_timeout: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub url_username: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub use_default_credential: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub use_proxy: Option<bool>,

    #[serde(alias = "user_name")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub username: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub validate_certs: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub wait_for_children: Option<bool>,
}
