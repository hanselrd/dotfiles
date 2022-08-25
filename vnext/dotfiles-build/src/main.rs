fn main() {
    println!(
        "{}",
        serde_yaml::to_string(&dft::Configuration {
            ..Default::default()
        })
        .unwrap()
    );
    println!(
        "{}",
        toml::to_string(&dft::Configuration {
            user: "bbuilder".to_string(),
            user_name: "Bob Builder".to_string(),
            user_email: "bobbuilder@gmail.com".to_string(),
            system: dft::System::Linux,
            preset: dft::Preset::Server,
            roles: vec![
                // dft::Role::Bspwm,
                // dft::Role::Dwm,
                // dft::Role::Polybar,
                // dft::Role::Runit,
                // dft::Role::Urxvt,
                dft::Role::Alacritty,
                dft::Role::Alsa,
                dft::Role::Backgrounds,
                dft::Role::Bin,
                dft::Role::Ccache,
                dft::Role::Chsh,
                dft::Role::Docker,
                dft::Role::Elm,
                dft::Role::Fonts,
                dft::Role::Gdb,
                dft::Role::Git,
                dft::Role::Gtk,
                dft::Role::Haskell,
                dft::Role::I3,
                dft::Role::I3Status,
                dft::Role::Kernel,
                dft::Role::Lua,
                dft::Role::Nodejs,
                dft::Role::Packages,
                dft::Role::Picom,
                dft::Role::Purescript,
                dft::Role::Python,
                dft::Role::Ranger,
                dft::Role::Rofi,
                dft::Role::Rust,
                dft::Role::Ssh,
                dft::Role::Sxhkd,
                dft::Role::Systemd,
                dft::Role::Theme,
                dft::Role::Tlp,
                dft::Role::Tmux,
                dft::Role::Vim,
                dft::Role::Vscode,
                dft::Role::Xinit,
                dft::Role::Xrandr,
                dft::Role::Xrdb,
                dft::Role::Zsh,
            ],
            ..Default::default()
        })
        .unwrap()
    );
}
