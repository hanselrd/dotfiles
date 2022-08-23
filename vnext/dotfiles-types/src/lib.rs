// #[derive(
//     Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_builder::Builder,
// )]
// #[builder(setter(into))]

pub mod preset;
pub mod privilege;
pub mod system;

pub use preset::*;
pub use privilege::*;
pub use system::*;
