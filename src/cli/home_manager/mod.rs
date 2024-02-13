pub mod bootstrap;
pub mod eject;

pub use bootstrap::BootstrapArgs;
use clap::{Args, Subcommand};
pub use eject::EjectArgs;

#[derive(Debug, Args)]
pub struct HomeManagerArgs {
    /// Home Manager profile (WIP)
    #[arg(short, long, default_value = "nixos-full")]
    pub profile: String,

    #[command(subcommand)]
    pub command: HomeManagerCommand,
}

#[derive(Debug, Subcommand)]
pub enum HomeManagerCommand {
    /// Bootstrap command
    Bootstrap(BootstrapArgs),

    /// Eject command
    Eject(EjectArgs),
}
