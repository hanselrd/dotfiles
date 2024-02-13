use serde::Serialize;
use strum::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Display)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Type {
    System,
    User,
}

pub trait Typeable {
    fn r#type(&self) -> Type;
}
