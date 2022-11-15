// use handlebars::Handlebars;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct Generator {
    pub tasks: HashMap<dft::Role, Vec<dfa::Task>>,
    // add commands

    // // main.yml
    // pub tasks: Vec<Task>,
    // // main.yml
    // pub handlers: Vec<String>,
    // // template.txt.j2
    // pub templates: Vec<String>,
    // // file.txt
    // pub files: Vec<String>,
    // // main.yml
    // pub vars: Vec<String>,
    // // main.yml
    // pub defaults: Vec<String>,
    // // main.yml
    // pub meta: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    #[allow(unused_variables)]
    pub fn add_template(
        &mut self,
        role: dft::Role,
        source: PathBuf,
        destination: PathBuf,
    ) -> &mut Self {
        self
    }
}

// use handlebars::Handlebars;

// pub fn example_handlebars(config: &dft::Configuration) {
//     let mut handlebars = Handlebars::new();
// handlebars.set_strict_mode(true);
//     handlebars.register_template_file("hello", "dotfiles-core/src/roles/common/files/hello.hbs")
//     .unwrap();

//     println!("{}", handlebars.render("hello", &config).unwrap());
// }
