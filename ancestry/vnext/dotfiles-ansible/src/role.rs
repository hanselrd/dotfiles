use crate::*;
use serde::Serialize;

#[derive(Debug, Default, Clone, PartialEq, Serialize)]
pub struct Role {
    // main.yml
    pub tasks: Vec<Task>,
    // main.yml
    pub handlers: Vec<String>,
    // template.txt.j2
    pub templates: Vec<String>,
    // file.txt
    pub files: Vec<String>,
    // main.yml
    pub vars: Vec<String>,
    // main.yml
    pub defaults: Vec<String>,
    // main.yml
    pub meta: Vec<String>,
}
