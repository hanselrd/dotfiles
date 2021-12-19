let External/Prelude = ../External/Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let Role/Metadata = ./Metadata/Record.partial.dhall

let Role/equal = ../../codegen/Lib/Role/equal.partial.dhall

let toMetadata
    : Role -> Role/Metadata.Type
    = \(role : Role) ->
        let metadata =
              merge
                { Alacritty = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Urxvt ]
                  }
                , Backgrounds = Role/Metadata::{=}
                , Bin = Role/Metadata::{=}
                , Bspwm = Role/Metadata::{ conflicts = [ Role.Dwm, Role.I3 ] }
                , Ccache = Role/Metadata::{=}
                , Chsh = Role/Metadata::{=}
                , Common = Role/Metadata::{=}
                , Dwm = Role/Metadata::{ conflicts = [ Role.Bspwm, Role.I3 ] }
                , Elm = Role/Metadata::{=}
                , Fonts = Role/Metadata::{=}
                , Gdb = Role/Metadata::{=}
                , Git = Role/Metadata::{=}
                , Gtk = Role/Metadata::{=}
                , Haskell = Role/Metadata::{=}
                , I3 = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Bspwm, Role.Dwm ]
                  }
                , I3status = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.I3 ]
                  }
                , Nodejs = Role/Metadata::{=}
                , Packages = Role/Metadata::{=}
                , Picom = Role/Metadata::{=}
                , Python = Role/Metadata::{=}
                , Ranger = Role/Metadata::{=}
                , Rofi = Role/Metadata::{=}
                , Runit = Role/Metadata::{ conflicts = [ Role.Systemd ] }
                , Rust = Role/Metadata::{=}
                , Ssh = Role/Metadata::{=}
                , Sxhkd = Role/Metadata::{=}
                , Systemd = Role/Metadata::{ conflicts = [ Role.Runit ] }
                , Theme = Role/Metadata::{=}
                , Tmux = Role/Metadata::{=}
                , Urxvt = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Alacritty ]
                  }
                , Vim = Role/Metadata::{=}
                , Vscode = Role/Metadata::{=}
                , Xinit = Role/Metadata::{=}
                , Xrandr = Role/Metadata::{=}
                , Xrdb = Role/Metadata::{=}
                , Zsh = Role/Metadata::{=}
                }
                role

        in      metadata
            //  { dependencies =
                      metadata.dependencies
                    # ( if    External/Prelude.Bool.not
                                (Role/equal role Role.Common)
                        then  [ Role.Common ]
                        else  [] : List Role
                      )
                }

in  toMetadata
