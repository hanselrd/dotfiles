let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let Role/Metadata = ./Metadata/Record.partial.dhall

let Role/equal = ../../codegen/Lib/Role/equal.partial.dhall

let env = ../../codegen/environment.partial.dhall

let toMetadata
    : Role -> Role/Metadata.Type
    = \(role : Role) ->
        let metadata =
              merge
                { Alacritty = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Fonts, Role.Theme ]
                  , conflicts = [ Role.Urxvt ]
                  }
                , Alsa = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Backgrounds = Role/Metadata::{=}
                , Bin = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Python ]
                  }
                , Bspwm = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Dwm, Role.I3 ]
                  }
                , Ccache = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Chsh = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Zsh ]
                  }
                , Common = Role/Metadata::{=}
                , Docker = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Dwm = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Bspwm, Role.I3 ]
                  }
                , Elm = Role/Metadata::{
                  , dependencies = [ Role.Zsh, Role.Nodejs ]
                  }
                , Fonts = Role/Metadata::{=}
                , Gdb = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Git = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Gtk = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Haskell = Role/Metadata::{ dependencies = [ Role.Zsh ] }
                , I3 = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Bspwm, Role.Dwm ]
                  }
                , I3status = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.I3 ]
                  , conflicts = [ Role.Polybar ]
                  }
                , Kernel = Role/Metadata::{=}
                , Lua = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Zsh ]
                  }
                , Nodejs = Role/Metadata::{ dependencies = [ Role.Zsh ] }
                , Packages = Role/Metadata::{=}
                , Picom = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Polybar = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.I3status ]
                  }
                , Purescript = Role/Metadata::{
                  , dependencies = [ Role.Zsh, Role.Nodejs ]
                  }
                , Python = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Ranger = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Rofi = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Runit = Role/Metadata::{
                  , dependencies = [ Role.Packages ]
                  , conflicts = [ Role.Systemd ]
                  }
                , Rust = Role/Metadata::{ dependencies = [ Role.Zsh ] }
                , Ssh = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Sxhkd = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Xrandr ]
                  }
                , Systemd = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Python ]
                  , conflicts = [ Role.Runit ]
                  }
                , Theme = Role/Metadata::{=}
                , Tlp = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Tmux = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Urxvt = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Xrdb ]
                  , conflicts = [ Role.Alacritty ]
                  }
                , Vim = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Tmux ]
                  }
                , Vscode = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Xinit = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Xrandr = Role/Metadata::{
                  , dependencies = [ Role.Packages, Role.Python ]
                  }
                , Xrdb = Role/Metadata::{ dependencies = [ Role.Packages ] }
                , Zsh = Role/Metadata::{ dependencies = [ Role.Packages ] }
                }
                role

        in      metadata
            //  { dependencies =
                    External/Prelude.List.filter
                      Role
                      ( \(role : Role) ->
                          External/Prelude.Bool.not
                            ( Prelude.List.contains
                                Role
                                role
                                Role/equal
                                env.unsafe_ignore_dependencies
                            )
                      )
                      (   metadata.dependencies
                        # ( if    External/Prelude.Bool.not
                                    (Role/equal role Role.Common)
                            then  [ Role.Common ]
                            else  [] : List Role
                          )
                      )
                }

in  toMetadata
