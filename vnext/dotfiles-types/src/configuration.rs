use crate::*;
use clap::Parser;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(
    Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Parser,
)]
pub struct Configuration {
    pub user: String,
    pub user_name: String,
    pub user_email: String,
    pub user_home_dir: PathBuf,
    pub user_cache_dir: PathBuf,
    pub user_config_dir: PathBuf,
    pub user_root_dir: PathBuf,
    pub user_temp_dir: PathBuf,
    #[clap(value_enum)]
    pub system: System,
    #[clap(value_enum)]
    pub preset: Preset,
    // pub package_manager: ???,
    // pub background: ???,
    // pub theme: ???,
    // pub font: ???,
    #[clap(value_enum)]
    pub unsafe_ignore_dependencies: Vec<Role>,
    #[clap(value_enum)]
    pub roles: Vec<Role>,
}
