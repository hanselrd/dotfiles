use clap::Args;

#[derive(Debug, Args)]
pub struct SecretArgs {
    /// arg0 argument
    #[arg(short, long)]
    arg0: Option<String>,
}
