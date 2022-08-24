use crate::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Configuration {
    pub user: String,
    pub user_name: String,
    pub user_email: String,
    pub user_home_dir: String,
    pub user_cache_dir: String,
    pub user_config_dir: String,
    pub user_root_dir: String,
    pub user_temp_dir: String,
    pub system: System,
    pub preset: Preset,
    pub package_manager: bool,
    pub background: bool,
    pub theme: bool,
    pub font: bool,
    pub unsafe_ignore_dependencies: Vec<bool>,
    pub roles: Vec<bool>,
}
