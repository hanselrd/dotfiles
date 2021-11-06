let Role = ../Role.partial.dhall

in  \(role : Role) ->
      merge
        { Alacritty = 0
        , Backgrounds = 1
        , Bin = 2
        , Bspwm = 3
        , Ccache = 4
        , Chsh = 5
        , Dwm = 6
        , Elm = 7
        , Fonts = 8
        , Gdb = 9
        , Git = 10
        , Gtk = 11
        , Haskell = 12
        , I3 = 13
        , I3status = 14
        , Nodejs = 15
        , Packages = 16
        , Picom = 17
        , Python = 18
        , Ranger = 19
        , Rofi = 20
        , Runit = 21
        , Rust = 22
        , Ssh = 23
        , Sxhkd = 24
        , Systemd = 25
        , Theme = 26
        , Tmux = 27
        , Urxvt = 28
        , Vim = 29
        , Vscode = 30
        , Xinit = 31
        , Xrandr = 32
        , Xrdb = 33
        , Zsh = 34
        }
        role
