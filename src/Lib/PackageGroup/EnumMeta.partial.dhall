let External/Prelude = ../External/Prelude.partial.dhall

let PackageGroup = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { Alacritty = False
      , Alsa = False
      , Android = False
      , Arandr = False
      , Bolt = False
      , Ccache = False
      , Clang = False
      , Cmake = False
      , Ctags = False
      , Cups = False
      , Dbeaver = False
      , Dhall = False
      , Feh = False
      , Ffmpeg = False
      , Flameshot = False
      , Fonts = False
      , Gdb = False
      , Glfw = False
      , Graphviz = False
      , Htop = False
      , I3 = False
      , Libreoffice = False
      , Lldb = False
      , Lxappearance = False
      , Mesa = False
      , Neofetch = False
      , Ninja = False
      , Nmap = False
      , Openresolv = False
      , Openssh = False
      , Openvpn = False
      , Other = False
      , Pandoc = False
      , Picom = False
      , Postgresql = False
      , Pulseaudio = False
      , Python = False
      , Ranger = False
      , Redshift = False
      , Rofi = False
      , Rsync = False
      , RxvtUnicode = False
      , Sdl2 = False
      , Shellcheck = False
      , Sshfs = False
      , Strace = False
      , Sxhkd = False
      , Tlp = False
      , Tmux = False
      , Tree = False
      , Udisks2 = False
      , Unzip = False
      , Upower = False
      , Vulkan = False
      , W3m = False
      , Wal = False
      , Xdg = False
      , Xorg = False
      , Xsel = False
      , YoutubeDl = False
      , Zsh = False
      }

let meta =
      { Alacritty = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Alacritty
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Alacritty = True }) packageGroup
        }
      , Alsa = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Alsa
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Alsa = True }) packageGroup
        }
      , Android = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Android
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Android = True }) packageGroup
        }
      , Arandr = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Arandr
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Arandr = True }) packageGroup
        }
      , Bolt = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Bolt
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Bolt = True }) packageGroup
        }
      , Ccache = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Ccache
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Ccache = True }) packageGroup
        }
      , Clang = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Clang
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Clang = True }) packageGroup
        }
      , Cmake = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Cmake
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Cmake = True }) packageGroup
        }
      , Ctags = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Ctags
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Ctags = True }) packageGroup
        }
      , Cups = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Cups
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Cups = True }) packageGroup
        }
      , Dbeaver = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Dbeaver
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Dbeaver = True }) packageGroup
        }
      , Dhall = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Dhall
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Dhall = True }) packageGroup
        }
      , Feh = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Feh
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Feh = True }) packageGroup
        }
      , Ffmpeg = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Ffmpeg
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Ffmpeg = True }) packageGroup
        }
      , Flameshot = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Flameshot
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Flameshot = True }) packageGroup
        }
      , Fonts = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Fonts
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Fonts = True }) packageGroup
        }
      , Gdb = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Gdb
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Gdb = True }) packageGroup
        }
      , Glfw = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Glfw
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Glfw = True }) packageGroup
        }
      , Graphviz = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Graphviz
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Graphviz = True }) packageGroup
        }
      , Htop = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Htop
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Htop = True }) packageGroup
        }
      , I3 = (EnumMeta PackageGroup)::{
        , value = PackageGroup.I3
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { I3 = True }) packageGroup
        }
      , Libreoffice = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Libreoffice
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Libreoffice = True }) packageGroup
        }
      , Lldb = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Lldb
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Lldb = True }) packageGroup
        }
      , Lxappearance = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Lxappearance
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Lxappearance = True }) packageGroup
        }
      , Mesa = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Mesa
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Mesa = True }) packageGroup
        }
      , Neofetch = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Neofetch
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Neofetch = True }) packageGroup
        }
      , Ninja = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Ninja
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Ninja = True }) packageGroup
        }
      , Nmap = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Nmap
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Nmap = True }) packageGroup
        }
      , Openresolv = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Openresolv
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Openresolv = True }) packageGroup
        }
      , Openssh = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Openssh
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Openssh = True }) packageGroup
        }
      , Openvpn = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Openvpn
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Openvpn = True }) packageGroup
        }
      , Other = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Other
        , sort = Some 99999
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Other = True }) packageGroup
        }
      , Pandoc = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Pandoc
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Pandoc = True }) packageGroup
        }
      , Picom = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Picom
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Picom = True }) packageGroup
        }
      , Postgresql = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Postgresql
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Postgresql = True }) packageGroup
        }
      , Pulseaudio = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Pulseaudio
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Pulseaudio = True }) packageGroup
        }
      , Python = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Python
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Python = True }) packageGroup
        }
      , Ranger = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Ranger
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Ranger = True }) packageGroup
        }
      , Redshift = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Redshift
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Redshift = True }) packageGroup
        }
      , Rofi = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Rofi
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Rofi = True }) packageGroup
        }
      , Rsync = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Rsync
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Rsync = True }) packageGroup
        }
      , RxvtUnicode = (EnumMeta PackageGroup)::{
        , value = PackageGroup.RxvtUnicode
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { RxvtUnicode = True }) packageGroup
        }
      , Sdl2 = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Sdl2
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Sdl2 = True }) packageGroup
        }
      , Shellcheck = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Shellcheck
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Shellcheck = True }) packageGroup
        }
      , Sshfs = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Sshfs
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Sshfs = True }) packageGroup
        }
      , Strace = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Strace
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Strace = True }) packageGroup
        }
      , Sxhkd = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Sxhkd
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Sxhkd = True }) packageGroup
        }
      , Tlp = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Tlp
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Tlp = True }) packageGroup
        }
      , Tmux = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Tmux
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Tmux = True }) packageGroup
        }
      , Tree = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Tree
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Tree = True }) packageGroup
        }
      , Udisks2 = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Udisks2
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Udisks2 = True }) packageGroup
        }
      , Unzip = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Unzip
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Unzip = True }) packageGroup
        }
      , Upower = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Upower
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Upower = True }) packageGroup
        }
      , Vulkan = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Vulkan
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Vulkan = True }) packageGroup
        }
      , W3m = (EnumMeta PackageGroup)::{
        , value = PackageGroup.W3m
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { W3m = True }) packageGroup
        }
      , Wal = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Wal
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Wal = True }) packageGroup
        }
      , Xdg = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Xdg
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Xdg = True }) packageGroup
        }
      , Xorg = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Xorg
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Xorg = True }) packageGroup
        }
      , Xsel = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Xsel
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Xsel = True }) packageGroup
        }
      , YoutubeDl = (EnumMeta PackageGroup)::{
        , value = PackageGroup.YoutubeDl
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { YoutubeDl = True }) packageGroup
        }
      , Zsh = (EnumMeta PackageGroup)::{
        , value = PackageGroup.Zsh
        , equal =
            \(packageGroup : PackageGroup) ->
              merge (default // { Zsh = True }) packageGroup
        }
      }

let validate = assert : merge meta PackageGroup.Alacritty === meta.Alacritty

in  External/Prelude.Map.values Text (EnumMeta PackageGroup).Type (toMap meta)
