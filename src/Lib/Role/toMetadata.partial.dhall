let Role/Enum = ./Enum.partial.dhall

let Role/Metadata = ./Metadata.partial.dhall

let toMetadata
    : Role/Enum -> Role/Metadata.Type
    = \(role : Role/Enum) ->
        merge
          { Alacritty = Role/Metadata::{ conflicts = [ Role/Enum.Urxvt ] }
          , Backgrounds = Role/Metadata::{=}
          , Bin = Role/Metadata::{=}
          , Bspwm = Role/Metadata::{
            , conflicts = [ Role/Enum.Dwm, Role/Enum.I3 ]
            }
          , Ccache = Role/Metadata::{=}
          , Chsh = Role/Metadata::{=}
          , Dwm = Role/Metadata::{
            , conflicts = [ Role/Enum.Bspwm, Role/Enum.I3 ]
            }
          , Elm = Role/Metadata::{=}
          , Fonts = Role/Metadata::{=}
          , Gdb = Role/Metadata::{=}
          , Git = Role/Metadata::{=}
          , Gtk = Role/Metadata::{=}
          , Haskell = Role/Metadata::{=}
          , I3 = Role/Metadata::{
            , dependencies = [ Role/Enum.Packages ]
            , conflicts = [ Role/Enum.Bspwm, Role/Enum.Dwm ]
            }
          , I3status = Role/Metadata::{ dependencies = [ Role/Enum.Packages, Role/Enum.I3 ] }
          , Nodejs = Role/Metadata::{=}
          , Packages = Role/Metadata::{=}
          , Picom = Role/Metadata::{=}
          , Python = Role/Metadata::{=}
          , Ranger = Role/Metadata::{=}
          , Rofi = Role/Metadata::{=}
          , Runit = Role/Metadata::{ conflicts = [ Role/Enum.Systemd ] }
          , Rust = Role/Metadata::{=}
          , Ssh = Role/Metadata::{=}
          , Sxhkd = Role/Metadata::{=}
          , Systemd = Role/Metadata::{ conflicts = [ Role/Enum.Runit ] }
          , Theme = Role/Metadata::{=}
          , Tmux = Role/Metadata::{=}
          , Urxvt = Role/Metadata::{ conflicts = [ Role/Enum.Alacritty ] }
          , Vim = Role/Metadata::{=}
          , Vscode = Role/Metadata::{=}
          , Xinit = Role/Metadata::{=}
          , Xrandr = Role/Metadata::{=}
          , Xrdb = Role/Metadata::{=}
          , Zsh = Role/Metadata::{=}
          }
          role

in  toMetadata
