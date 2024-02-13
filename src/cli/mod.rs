pub mod docker_compose;
pub mod environment;
pub mod home_manager;
pub mod homeage;
pub mod template;

use clap::{Parser, Subcommand, ValueEnum};
pub use docker_compose::*;
pub use environment::*;
pub use home_manager::*;
pub use homeage::*;
pub use template::*;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Run without affecting the system
    #[arg(short, long)]
    pub dryrun: bool,

    /// Log level
    #[arg(short, long, value_enum, default_value_t = LogLevel::Info)]
    pub log_level: LogLevel,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum LogLevel {
    Error = 1,
    Warn,
    Info,
    Debug,
    Trace,
}

impl From<LogLevel> for log::LevelFilter {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Error => log::LevelFilter::Error,
            LogLevel::Warn => log::LevelFilter::Warn,
            LogLevel::Info => log::LevelFilter::Info,
            LogLevel::Debug => log::LevelFilter::Debug,
            LogLevel::Trace => log::LevelFilter::Trace,
        }
    }
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Home Manager command
    HomeManager(HomeManagerArgs),

    /// Homeage command
    Homeage(HomeageArgs),

    /// Environment command
    Environment(EnvironmentArgs),

    /// Template command
    Template(TemplateArgs),

    /// Docker Compose command
    DockerCompose(DockerComposeArgs),
}
