use crate::*;
use config::{Config, ConfigError, Environment, File};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::env;
use std::path::PathBuf;

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Configuration {
    pub user: String,
    pub user_name: String,
    pub user_email: String,
    pub user_home_dir: PathBuf,
    pub user_cache_dir: PathBuf,
    pub user_config_dir: PathBuf,
    pub user_root_dir: PathBuf,
    pub user_temp_dir: PathBuf,
    pub system: System,
    pub preset: Preset,
    // // pub package_manager: ???,
    // // pub background: ???,
    // // pub theme: ???,
    // // pub font: ???,
    pub unsafe_ignore_dependencies: HashSet<Role>,
    pub roles: HashSet<Role>,
}

impl Configuration {
    pub fn new() -> Result<Self, ConfigError> {
        let run_mode = env::var("RUN_MODE").unwrap_or_else(|_| String::from("development"));

        let config = Config::builder()
            .add_source(File::with_name("config/default"))
            .add_source(File::with_name(&format!("config/{}", run_mode)).required(false))
            .add_source(File::with_name("config/local").required(false))
            .add_source(Environment::with_prefix("DOTFILES"))
            .build()?;

        config.try_deserialize()
    }
}
