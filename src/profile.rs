use crate::{Nameable, Typeable};

pub trait Profile: Typeable + Nameable {
    fn profile(&self) -> String;
    fn name(&self) -> String {
        format!("{}.{}", self.r#type(), self.profile())
    }
}
