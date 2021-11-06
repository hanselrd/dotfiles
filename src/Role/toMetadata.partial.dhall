let Role = ../Role.partial.dhall

let RoleMetadata = ./Metadata.partial.dhall

in  \(role : Role) ->
      merge
        { Alacritty = RoleMetadata::{ conflicts = [ Role.Urxvt ] }
        , Backgrounds = RoleMetadata::{=}
        , Bin = RoleMetadata::{=}
        , Bspwm = RoleMetadata::{ conflicts = [ Role.Dwm, Role.I3 ] }
        , Ccache = RoleMetadata::{=}
        , Chsh = RoleMetadata::{=}
        , Dwm = RoleMetadata::{ conflicts = [ Role.Bspwm, Role.I3 ] }
        , Elm = RoleMetadata::{=}
        , Fonts = RoleMetadata::{=}
        , Gdb = RoleMetadata::{=}
        , Git = RoleMetadata::{=}
        , Gtk = RoleMetadata::{=}
        , Haskell = RoleMetadata::{=}
        , I3 = RoleMetadata::{
          , dependencies = [ Role.I3status ]
          , conflicts = [ Role.Bspwm, Role.Dwm ]
          }
        , I3status = RoleMetadata::{ dependencies = [ Role.I3 ] }
        , Nodejs = RoleMetadata::{=}
        , Packages = RoleMetadata::{=}
        , Picom = RoleMetadata::{=}
        , Python = RoleMetadata::{=}
        , Ranger = RoleMetadata::{=}
        , Rofi = RoleMetadata::{=}
        , Runit = RoleMetadata::{ conflicts = [ Role.Systemd ] }
        , Rust = RoleMetadata::{=}
        , Ssh = RoleMetadata::{=}
        , Sxhkd = RoleMetadata::{=}
        , Systemd = RoleMetadata::{ conflicts = [ Role.Runit ] }
        , Theme = RoleMetadata::{=}
        , Tmux = RoleMetadata::{=}
        , Urxvt = RoleMetadata::{ conflicts = [ Role.Alacritty ] }
        , Vim = RoleMetadata::{=}
        , Vscode = RoleMetadata::{=}
        , Xinit = RoleMetadata::{=}
        , Xrandr = RoleMetadata::{=}
        , Xrdb = RoleMetadata::{=}
        , Zsh = RoleMetadata::{=}
        }
        role
