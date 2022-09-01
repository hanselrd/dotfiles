use lazy_static::lazy_static;

lazy_static! {
    pub static ref CONFIG: dft::Configuration = dft::Configuration::new().unwrap();
}
