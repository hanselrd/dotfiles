let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let Directory = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let env = ../../codegen/environment.partial.dhall

let default =
      { Alacritty = False
      , Alsa = False
      , Background = False
      , Bin = False
      , Ccache = False
      , Font = False
      , Gdb1 = False
      , Gdb2 = False
      , Git = False
      , Gtk1 = False
      , Gtk2 = False
      , I3 = False
      , I3status = False
      , Picom = False
      , Polybar = False
      , Ranger = False
      , Rofi = False
      , Rust = False
      , Ssh = False
      , Sxhkd = False
      , Systemd1 = False
      , Systemd2 = False
      , Theme = False
      , Tlp = False
      , Tmux1 = False
      , Tmux2 = False
      , Urxvt = False
      , Vim1 = False
      , Vim2 = False
      , Vscode = False
      , Xinit = False
      , Xrandr = False
      , Xrdb = False
      , Zsh = False
      }

let meta =
      { Alacritty = (EnumMeta Directory)::{
        , value = Directory.Alacritty
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/alacritty")
        , equal =
            \(directory : Directory) ->
              merge (default // { Alacritty = True }) directory
        }
      , Alsa = (EnumMeta Directory)::{
        , value = Directory.Alsa
        , text = Some
            (Prelude.Text.pathify "${env.user_root_dir}/etc/modprobe.d")
        , equal =
            \(directory : Directory) ->
              merge (default // { Alsa = True }) directory
        }
      , Background = (EnumMeta Directory)::{
        , value = Directory.Background
        , text = Some
            ( Prelude.Text.pathify
                "${env.user_root_dir}/usr/local/share/backgrounds"
            )
        , equal =
            \(directory : Directory) ->
              merge (default // { Background = True }) directory
        }
      , Bin = (EnumMeta Directory)::{
        , value = Directory.Bin
        , text = Some
            (Prelude.Text.pathify "${env.user_root_dir}/usr/local/bin")
        , equal =
            \(directory : Directory) ->
              merge (default // { Bin = True }) directory
        }
      , Ccache = (EnumMeta Directory)::{
        , value = Directory.Ccache
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}/.ccache")
        , equal =
            \(directory : Directory) ->
              merge (default // { Ccache = True }) directory
        }
      , Font = (EnumMeta Directory)::{
        , value = Directory.Font
        , text = Some
            (Prelude.Text.pathify "${env.user_root_dir}/usr/local/share/fonts")
        , equal =
            \(directory : Directory) ->
              merge (default // { Font = True }) directory
        }
      , Gdb1 = (EnumMeta Directory)::{
        , value = Directory.Gdb1
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Gdb1 = True }) directory
        }
      , Gdb2 = (EnumMeta Directory)::{
        , value = Directory.Gdb2
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}/.gdb")
        , equal =
            \(directory : Directory) ->
              merge (default // { Gdb2 = True }) directory
        }
      , Git = (EnumMeta Directory)::{
        , value = Directory.Git
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Git = True }) directory
        }
      , Gtk1 = (EnumMeta Directory)::{
        , value = Directory.Gtk1
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Gtk1 = True }) directory
        }
      , Gtk2 = (EnumMeta Directory)::{
        , value = Directory.Gtk2
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/gtk-3.0")
        , equal =
            \(directory : Directory) ->
              merge (default // { Gtk2 = True }) directory
        }
      , I3 = (EnumMeta Directory)::{
        , value = Directory.I3
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/i3")
        , equal =
            \(directory : Directory) ->
              merge (default // { I3 = True }) directory
        }
      , I3status = (EnumMeta Directory)::{
        , value = Directory.I3status
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/i3status")
        , equal =
            \(directory : Directory) ->
              merge (default // { I3status = True }) directory
        }
      , Picom = (EnumMeta Directory)::{
        , value = Directory.Picom
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/picom")
        , equal =
            \(directory : Directory) ->
              merge (default // { Picom = True }) directory
        }
      , Polybar = (EnumMeta Directory)::{
        , value = Directory.Polybar
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/polybar")
        , equal =
            \(directory : Directory) ->
              merge (default // { Polybar = True }) directory
        }
      , Ranger = (EnumMeta Directory)::{
        , value = Directory.Ranger
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/ranger")
        , equal =
            \(directory : Directory) ->
              merge (default // { Ranger = True }) directory
        }
      , Rofi = (EnumMeta Directory)::{
        , value = Directory.Rofi
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/rofi")
        , equal =
            \(directory : Directory) ->
              merge (default // { Rofi = True }) directory
        }
      , Rust = (EnumMeta Directory)::{
        , value = Directory.Rust
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/rustfmt")
        , equal =
            \(directory : Directory) ->
              merge (default // { Rust = True }) directory
        }
      , Ssh = (EnumMeta Directory)::{
        , value = Directory.Ssh
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}/.ssh")
        , equal =
            \(directory : Directory) ->
              merge (default // { Ssh = True }) directory
        }
      , Sxhkd = (EnumMeta Directory)::{
        , value = Directory.Sxhkd
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/sxhkd")
        , equal =
            \(directory : Directory) ->
              merge (default // { Sxhkd = True }) directory
        }
      , Systemd1 = (EnumMeta Directory)::{
        , value = Directory.Systemd1
        , text = Some
            (Prelude.Text.pathify "${env.user_config_dir}/systemd/user")
        , equal =
            \(directory : Directory) ->
              merge (default // { Systemd1 = True }) directory
        }
      , Systemd2 = (EnumMeta Directory)::{
        , value = Directory.Systemd2
        , text = Some
            (Prelude.Text.pathify "${env.user_cache_dir}/dotfiles/systemd")
        , equal =
            \(directory : Directory) ->
              merge (default // { Systemd2 = True }) directory
        }
      , Theme = (EnumMeta Directory)::{
        , value = Directory.Theme
        , text = Some
            (Prelude.Text.pathify "${env.user_config_dir}/wal/templates")
        , equal =
            \(directory : Directory) ->
              merge (default // { Theme = True }) directory
        }
      , Tlp = (EnumMeta Directory)::{
        , value = Directory.Tlp
        , text = Some (Prelude.Text.pathify "${env.user_root_dir}/etc")
        , equal =
            \(directory : Directory) ->
              merge (default // { Tlp = True }) directory
        }
      , Tmux1 = (EnumMeta Directory)::{
        , value = Directory.Tmux1
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Tmux1 = True }) directory
        }
      , Tmux2 = (EnumMeta Directory)::{
        , value = Directory.Tmux2
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}/.tmux")
        , equal =
            \(directory : Directory) ->
              merge (default // { Tmux2 = True }) directory
        }
      , Urxvt = (EnumMeta Directory)::{
        , value = Directory.Urxvt
        , text = Some
            (Prelude.Text.pathify "${env.user_home_dir}/.Xresources.d")
        , equal =
            \(directory : Directory) ->
              merge (default // { Urxvt = True }) directory
        }
      , Vim1 = (EnumMeta Directory)::{
        , value = Directory.Vim1
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Vim1 = True }) directory
        }
      , Vim2 = (EnumMeta Directory)::{
        , value = Directory.Vim2
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}/.vim")
        , equal =
            \(directory : Directory) ->
              merge (default // { Vim2 = True }) directory
        }
      , Vscode = (EnumMeta Directory)::{
        , value = Directory.Vscode
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/Code/User")
        , equal =
            \(directory : Directory) ->
              merge (default // { Vscode = True }) directory
        }
      , Xinit = (EnumMeta Directory)::{
        , value = Directory.Xinit
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Xinit = True }) directory
        }
      , Xrandr = (EnumMeta Directory)::{
        , value = Directory.Xrandr
        , text = Some
            (Prelude.Text.pathify "${env.user_home_dir}/.screenlayout")
        , equal =
            \(directory : Directory) ->
              merge (default // { Xrandr = True }) directory
        }
      , Xrdb = (EnumMeta Directory)::{
        , value = Directory.Xrdb
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Xrdb = True }) directory
        }
      , Zsh = (EnumMeta Directory)::{
        , value = Directory.Zsh
        , text = Some (Prelude.Text.pathify "${env.user_home_dir}")
        , equal =
            \(directory : Directory) ->
              merge (default // { Zsh = True }) directory
        }
      }

let validate = assert : merge meta Directory.Alacritty === meta.Alacritty

in  External/Prelude.Map.values Text (EnumMeta Directory).Type (toMap meta)
