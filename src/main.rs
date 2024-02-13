use clap::Parser;
use dotfiles::{logger, Cli};

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    logger::init(&cli);

    log::trace!("trace");
    log::debug!("debug");
    log::info!("info");
    log::warn!("warn");
    log::error!("error");

    log::info!("\ncli= {:#?}", cli)

    // if let Some(name) = cli.name.as_deref() {
    //     println!("Value for name: {name}");
    // }

    // if let Some(config_path) = cli.config.as_deref() {
    //     println!("Value for config: {}", config_path.display());
    // }

    // match cli.debug {
    //     0 => println!("Debug mode is off"),
    //     1 => println!("Debug mode is kind of on"),
    //     2 => println!("Debug mode is on"),
    //     _ => println!("Don't be crazy"),
    // }

    // match &cli.command {
    //     // Some(Command::Test { list }) => {
    //     //     if *list {
    //     //         println!("Printing testing lists...");
    //     //     } else {
    //     //         println!("Not printing testing lists...");
    //     //     }
    //     // }
    //     Some(Command::HomeManager(_args)) => {}
    //     Some(_) => {
    //         println!("Called some command")
    //     }
    //     None => {
    //         println!("No subcommand specified")
    //     }
    // }
}
