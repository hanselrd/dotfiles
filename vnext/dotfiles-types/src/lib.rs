// #[derive(
//     Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_builder::Builder,
// )]
// #[builder(setter(into))]

pub mod configuration;
pub mod privilege;
pub mod system;

pub use configuration::*;
pub use privilege::*;
pub use system::*;
