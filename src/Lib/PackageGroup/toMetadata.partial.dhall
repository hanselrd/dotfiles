let PackageGroup = ./Enum.partial.dhall

let PackageGroup/Metadata = ./Metadata/Record.partial.dhall

let Package = ../Package/Record.partial.dhall

let Package/Flag = ../Package/Flag/Enum.partial.dhall

let PackageManager = ../PackageManager/Enum.partial.dhall

let toMetadata
    : PackageGroup -> PackageManager -> PackageGroup/Metadata.Type
    = \(packageGroup : PackageGroup) ->
      \(packageManager : PackageManager) ->
        merge
          { Alacritty =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "alacritty" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Alsa =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "alsa-utils" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Android =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "android-tools" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Arandr =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "arandr" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "arandr" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Ccache =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ccache" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ccache" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Clang =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "clang" }, Package::{ name = "llvm" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "clang" }
                    , Package::{ name = "clang-tools-extra" }
                    ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Cmake =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "cmake" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "cmake" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Ctags =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ctags" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ctags" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Cups =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "cups" }
                    , Package::{ name = "cups-pdf" }
                    , Package::{ name = "gutenprint" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Dbeaver =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "dbeaver" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Dhall =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "dhall" }
                    , Package::{ name = "dhall-lsp-server" }
                    , Package::{ name = "dhall-json" }
                    , Package::{ name = "dhall-yaml" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Feh =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "feh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "feh" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Ffmpeg =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ffmpeg" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Flameshot =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "flameshot" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "flameshot" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Fonts =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "noto-fonts" }
                    , Package::{ name = "noto-fonts-cjk" }
                    , Package::{ name = "noto-fonts-emoji" }
                    , Package::{ name = "noto-fonts-extra" }
                    , Package::{ name = "ttf-liberation" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Gdb =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "gdb" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Glfw =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "glfw-x11" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "glfw-devel" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Graphviz =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "graphviz" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Htop =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "htop" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "htop" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , I3 =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "i3-gaps" }
                    , Package::{
                      , name = "i3lock-color"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{ name = "i3status-rust" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "i3-gaps" }
                    , Package::{ name = "i3status-rs" }
                    ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Libreoffice =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "libreoffice-fresh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Lldb =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "lldb" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Lxappearance =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "lxappearance" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Mesa =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "mesa-demos" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Nasm =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "nasm" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Neofetch =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "neofetch" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "neofetch" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Ninja =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ninja" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ninja-build" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Nmap =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "nmap" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Openresolv =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openresolv" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Openssh =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openssh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Openvpn =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openvpn" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openvpn" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Other =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "archinstall" }
                    , Package::{ name = "archiso" }
                    , Package::{
                      , name = "brave-bin"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "downgrade"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "numix-gtk-theme"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "numix-icon-theme-pack-git"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "objconv"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{ name = "pacman-contrib" }
                    , Package::{
                      , name = "polybar"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{ name = "reflector" }
                    , Package::{ name = "rpm-tools" }
                    , Package::{
                      , name = "snap-pac-grub"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "ventoy-bin"
                      , flag = Some Package/Flag.Aur
                      }
                    , Package::{
                      , name = "visual-studio-code-bin"
                      , flag = Some Package/Flag.Aur
                      }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "chromium" }
                    , Package::{ name = "dnf-plugin-system-upgrade" }
                    , Package::{ name = "dnf-plugins-extras-tracer" }
                    , Package::{ name = "fuse-exfat" }
                    , Package::{ name = "numix-gtk-theme" }
                    , Package::{ name = "numix-icon-theme-circle" }
                    , Package::{ name = "numix-icon-theme-square" }
                    , Package::{ name = "numix-icon-theme" }
                    , Package::{ name = "polybar" }
                    , Package::{ name = "python3-dnf-plugin-snapper" }
                    , Package::{ name = "rpmconf" }
                    , Package::{ name = "snapper" }
                    ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Pandoc =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "pandoc" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Picom =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "picom" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "picom" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Postgresql =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "postgresql" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Pulseaudio =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "pulseaudio" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Python =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "python-pip" }
                    , Package::{ name = "python2" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "python3-devel" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Ranger =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ranger" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ranger" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Redshift =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "redshift" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "redshift" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Rofi =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rofi" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rofi" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Rsync =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rsync" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , RxvtUnicode =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rxvt-unicode" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rxvt-unicode" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Sdl2 =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "sdl2" }
                    , Package::{ name = "sdl2_image" }
                    , Package::{ name = "sdl2_mixer" }
                    , Package::{ name = "sdl2_net" }
                    , Package::{ name = "sdl2_ttf" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "SDL2-devel" }
                    , Package::{ name = "SDL2_image-devel" }
                    , Package::{ name = "SDL2_mixer-devel" }
                    , Package::{ name = "SDL2_net-devel" }
                    , Package::{ name = "SDL2_ttf-devel" }
                    ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Shellcheck =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "shellcheck" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ShellCheck" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Sshfs =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "sshfs" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Strace =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "strace" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "strace" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Sxhkd =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "sxhkd" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "sxhkd" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Tmux =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "tmux" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "tmux" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Tree =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "tree" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Udisks2 =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "udisks2" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Unzip =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "unzip" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Upower =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "upower" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Vulkan =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "vulkan-intel" }
                    , Package::{ name = "vulkan-tools" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , W3m =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "w3m" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Wal =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "python-pywal" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Xdg =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "xdg-user-dirs" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Xorg =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "xorg" }
                    , Package::{ name = "xorg-apps" }
                    , Package::{ name = "xorg-drivers" }
                    , Package::{ name = "xorg-fonts" }
                    , Package::{ name = "xorg-xinit" }
                    ]
                  , absent = [ Package::{ name = "xf86-input-synaptics" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Xsel =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "xsel" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , YoutubeDl =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "youtube-dl" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          , Zsh =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "zsh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "zsh" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                packageManager
          }
          packageGroup

in  toMetadata
