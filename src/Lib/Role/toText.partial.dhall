let Role = ./Enum.partial.dhall

let toText
    : Role -> Text
    = \(role : Role) ->
        merge
          { Alacritty = "alacritty"
          , Backgrounds = "backgrounds"
          , Bin = "bin"
          , Bspwm = "bspwm"
          , Ccache = "ccache"
          , Chsh = "chsh"
          , Dwm = "dwm"
          , Elm = "elm"
          , Fonts = "fonts"
          , Gdb = "gdb"
          , Git = "git"
          , Gtk = "gtk"
          , Haskell = "haskell"
          , I3 = "i3"
          , I3status = "i3status"
          , Nodejs = "nodejs"
          , Packages = "packages"
          , Picom = "picom"
          , Python = "python"
          , Ranger = "ranger"
          , Rofi = "rofi"
          , Runit = "runit"
          , Rust = "rust"
          , Ssh = "ssh"
          , Sxhkd = "sxhkd"
          , Systemd = "systemd"
          , Theme = "theme"
          , Tmux = "tmux"
          , Urxvt = "urxvt"
          , Vim = "vim"
          , Vscode = "vscode"
          , Xinit = "xinit"
          , Xrandr = "xrandr"
          , Xrdb = "xrdb"
          , Zsh = "zsh"
          }
          role

in  toText
