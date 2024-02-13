pub mod key;
pub mod secret;

use clap::{Args, Subcommand};
pub use key::KeyArgs;
pub use secret::SecretArgs;

#[derive(Debug, Args)]
pub struct HomeageArgs {
    #[command(subcommand)]
    pub command: HomeageCommand,
}

#[derive(Debug, Subcommand)]
pub enum HomeageCommand {
    /// Key command
    Key(KeyArgs),

    /// Secret command
    Secret(SecretArgs),
}
