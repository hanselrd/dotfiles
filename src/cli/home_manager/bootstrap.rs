use clap::Args;

#[derive(Debug, Args)]
pub struct BootstrapArgs {
    /// arg0 argument
    #[arg(short, long)]
    arg0: Option<String>,
}
