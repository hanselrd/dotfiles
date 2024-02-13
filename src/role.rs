use crate::{Nameable, Typeable};

pub trait Role: Typeable + Nameable {
    fn role(&self) -> String;
    fn name(&self) -> String {
        format!("{}.{}", self.r#type(), self.role())
    }
}
