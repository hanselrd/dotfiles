let External/Prelude = ../External/Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { Alacritty = False
      , Backgrounds = False
      , Bin = False
      , Bspwm = False
      , Ccache = False
      , Chsh = False
      , Dwm = False
      , Elm = False
      , Fonts = False
      , Gdb = False
      , Git = False
      , Gtk = False
      , Haskell = False
      , I3 = False
      , I3status = False
      , Nodejs = False
      , Packages = False
      , Picom = False
      , Python = False
      , Ranger = False
      , Rofi = False
      , Runit = False
      , Rust = False
      , Ssh = False
      , Sxhkd = False
      , Systemd = False
      , Theme = False
      , Tmux = False
      , Urxvt = False
      , Vim = False
      , Vscode = False
      , Xinit = False
      , Xrandr = False
      , Xrdb = False
      , Zsh = False
      }

let meta =
      { Alacritty = (EnumMeta Role)::{
        , value = Role.Alacritty
        , text = "alacritty"
        , equal = \(role : Role) -> merge (default // { Alacritty = True }) role
        }
      , Backgrounds = (EnumMeta Role)::{
        , value = Role.Backgrounds
        , text = "backgrounds"
        , equal =
            \(role : Role) -> merge (default // { Backgrounds = True }) role
        }
      , Bin = (EnumMeta Role)::{
        , value = Role.Bin
        , text = "bin"
        , equal = \(role : Role) -> merge (default // { Bin = True }) role
        }
      , Bspwm = (EnumMeta Role)::{
        , value = Role.Bspwm
        , text = "bspwm"
        , equal = \(role : Role) -> merge (default // { Bspwm = True }) role
        }
      , Ccache = (EnumMeta Role)::{
        , value = Role.Ccache
        , text = "ccache"
        , equal = \(role : Role) -> merge (default // { Ccache = True }) role
        }
      , Chsh = (EnumMeta Role)::{
        , value = Role.Chsh
        , text = "chsh"
        , equal = \(role : Role) -> merge (default // { Chsh = True }) role
        }
      , Dwm = (EnumMeta Role)::{
        , value = Role.Dwm
        , text = "dwm"
        , equal = \(role : Role) -> merge (default // { Dwm = True }) role
        }
      , Elm = (EnumMeta Role)::{
        , value = Role.Elm
        , text = "elm"
        , equal = \(role : Role) -> merge (default // { Elm = True }) role
        }
      , Fonts = (EnumMeta Role)::{
        , value = Role.Fonts
        , text = "fonts"
        , equal = \(role : Role) -> merge (default // { Fonts = True }) role
        }
      , Gdb = (EnumMeta Role)::{
        , value = Role.Gdb
        , text = "gdb"
        , equal = \(role : Role) -> merge (default // { Gdb = True }) role
        }
      , Git = (EnumMeta Role)::{
        , value = Role.Git
        , text = "git"
        , equal = \(role : Role) -> merge (default // { Git = True }) role
        }
      , Gtk = (EnumMeta Role)::{
        , value = Role.Gtk
        , text = "gtk"
        , equal = \(role : Role) -> merge (default // { Gtk = True }) role
        }
      , Haskell = (EnumMeta Role)::{
        , value = Role.Haskell
        , text = "haskell"
        , equal = \(role : Role) -> merge (default // { Haskell = True }) role
        }
      , I3 = (EnumMeta Role)::{
        , value = Role.I3
        , text = "i3"
        , equal = \(role : Role) -> merge (default // { I3 = True }) role
        }
      , I3status = (EnumMeta Role)::{
        , value = Role.I3status
        , text = "i3status"
        , equal = \(role : Role) -> merge (default // { I3status = True }) role
        }
      , Nodejs = (EnumMeta Role)::{
        , value = Role.Nodejs
        , text = "nodejs"
        , equal = \(role : Role) -> merge (default // { Nodejs = True }) role
        }
      , Packages = (EnumMeta Role)::{
        , value = Role.Packages
        , text = "packages"
        , equal = \(role : Role) -> merge (default // { Packages = True }) role
        }
      , Picom = (EnumMeta Role)::{
        , value = Role.Picom
        , text = "picom"
        , equal = \(role : Role) -> merge (default // { Picom = True }) role
        }
      , Python = (EnumMeta Role)::{
        , value = Role.Python
        , text = "python"
        , equal = \(role : Role) -> merge (default // { Python = True }) role
        }
      , Ranger = (EnumMeta Role)::{
        , value = Role.Ranger
        , text = "ranger"
        , equal = \(role : Role) -> merge (default // { Ranger = True }) role
        }
      , Rofi = (EnumMeta Role)::{
        , value = Role.Rofi
        , text = "rofi"
        , equal = \(role : Role) -> merge (default // { Rofi = True }) role
        }
      , Runit = (EnumMeta Role)::{
        , value = Role.Runit
        , text = "runit"
        , equal = \(role : Role) -> merge (default // { Runit = True }) role
        }
      , Rust = (EnumMeta Role)::{
        , value = Role.Rust
        , text = "rust"
        , equal = \(role : Role) -> merge (default // { Rust = True }) role
        }
      , Ssh = (EnumMeta Role)::{
        , value = Role.Ssh
        , text = "ssh"
        , equal = \(role : Role) -> merge (default // { Ssh = True }) role
        }
      , Sxhkd = (EnumMeta Role)::{
        , value = Role.Sxhkd
        , text = "sxhkd"
        , equal = \(role : Role) -> merge (default // { Sxhkd = True }) role
        }
      , Systemd = (EnumMeta Role)::{
        , value = Role.Systemd
        , text = "systemd"
        , equal = \(role : Role) -> merge (default // { Systemd = True }) role
        }
      , Theme = (EnumMeta Role)::{
        , value = Role.Theme
        , text = "theme"
        , equal = \(role : Role) -> merge (default // { Theme = True }) role
        }
      , Tmux = (EnumMeta Role)::{
        , value = Role.Tmux
        , text = "tmux"
        , equal = \(role : Role) -> merge (default // { Tmux = True }) role
        }
      , Urxvt = (EnumMeta Role)::{
        , value = Role.Urxvt
        , text = "urxvt"
        , equal = \(role : Role) -> merge (default // { Urxvt = True }) role
        }
      , Vim = (EnumMeta Role)::{
        , value = Role.Vim
        , text = "vim"
        , equal = \(role : Role) -> merge (default // { Vim = True }) role
        }
      , Vscode = (EnumMeta Role)::{
        , value = Role.Vscode
        , text = "vscode"
        , equal = \(role : Role) -> merge (default // { Vscode = True }) role
        }
      , Xinit = (EnumMeta Role)::{
        , value = Role.Xinit
        , text = "xinit"
        , equal = \(role : Role) -> merge (default // { Xinit = True }) role
        }
      , Xrandr = (EnumMeta Role)::{
        , value = Role.Xrandr
        , text = "xrandr"
        , equal = \(role : Role) -> merge (default // { Xrandr = True }) role
        }
      , Xrdb = (EnumMeta Role)::{
        , value = Role.Xrdb
        , text = "xrdb"
        , equal = \(role : Role) -> merge (default // { Xrdb = True }) role
        }
      , Zsh = (EnumMeta Role)::{
        , value = Role.Zsh
        , text = "zsh"
        , equal = \(role : Role) -> merge (default // { Zsh = True }) role
        }
      }

let validate = assert : merge meta Role.Alacritty === meta.Alacritty

in  External/Prelude.Map.values Text (EnumMeta Role).Type (toMap meta)
