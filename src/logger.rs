use log4rs::append::console::{ConsoleAppender, Target};
use log4rs::config::{Appender, Config, Root};
use log4rs::encode::pattern::PatternEncoder;

use crate::Cli;

pub fn init(cli: &Cli) {
    let stderr = ConsoleAppender::builder()
        .target(Target::Stderr)
        .encoder(Box::new(PatternEncoder::new(
            "{d(%Y-%m-%d %H:%M:%S)} [{M}/{T:<8.8}] {f:>25.25}:{L:<5} {h({l:<5.5})}| {m}{n}",
        )))
        .build();

    let config = Config::builder().appender(Appender::builder().build("stderr", Box::new(stderr)))
                                  .build(Root::builder().appender("stderr")
                                                        .build(cli.log_level.into()))
                                  .unwrap();

    log4rs::init_config(config).unwrap();
}
