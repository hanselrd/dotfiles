let PackageGroup = ./Enum.partial.dhall

let PackageGroup/Metadata = ./Metadata/Record.partial.dhall

let Package = ../Package/Record.partial.dhall

let PackageFlag = ../PackageFlag/Enum.partial.dhall

let env = ../../codegen/environment.partial.dhall

let toMetadata
    : PackageGroup -> PackageGroup/Metadata.Type
    = \(packageGroup : PackageGroup) ->
        merge
          { Alacritty =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "alacritty" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Alsa =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "alsa-utils" }
                    , Package::{ name = "sof-firmware" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Android =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "android-tools" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Bat =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "bat" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rust-bat" } ]
                  }
                , Apt = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rust-bat" } ]
                  }
                }
                env.package_manager
          , Bolt =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "bolt" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
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
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Dbeaver =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "dbeaver" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Docker =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "docker" }
                    , Package::{ name = "docker-compose" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Emscripten =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "emscripten" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Exa =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "exa" } ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "exa" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Ffmpeg =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "ffmpeg" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Gdb =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "gdb" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Git =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "git" }
                    , Package::{ name = "git-filter-repo" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Graphviz =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "graphviz" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , I3 =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "i3-gaps" }
                    , Package::{
                      , name = "i3lock-color"
                      , flag = Some PackageFlag.Aur
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
                env.package_manager
          , Libreoffice =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "libreoffice-fresh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Lldb =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "lldb" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Lua =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "lua" }
                    , Package::{ name = "luarocks" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Lxappearance =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "lxappearance" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Mesa =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "mesa-demos" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Nmap =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "nmap" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Openresolv =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openresolv" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Openssh =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "openssh" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Other =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "archinstall" }
                    , Package::{ name = "archiso" }
                    , Package::{
                      , name = "brave-bin"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{
                      , name = "downgrade"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{
                      , name = "numix-gtk-theme"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{
                      , name = "numix-icon-theme-pack-git"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{ name = "objconv", flag = Some PackageFlag.Aur }
                    , Package::{ name = "pacman-contrib" }
                    , Package::{ name = "polybar", flag = Some PackageFlag.Aur }
                    , Package::{ name = "reflector" }
                    , Package::{ name = "rpm-tools" }
                    , Package::{
                      , name = "snap-pac-grub"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{
                      , name = "ventoy-bin"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{
                      , name = "visual-studio-code-bin"
                      , flag = Some PackageFlag.Aur
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
                env.package_manager
          , Pandoc =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "pandoc" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Postgresql =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "postgresql" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Pulseaudio =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "pulseaudio" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Python =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "python" }
                    , Package::{ name = "python-pip" }
                    , Package::{ name = "python2" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "python3-devel" } ]
                  }
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Rsync =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "rsync" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Sshfs =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "sshfs" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
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
                env.package_manager
          , Tlp =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "tlp" }
                    , Package::{ name = "tlp-rdw" }
                    , Package::{
                      , name = "tlpui-git"
                      , flag = Some PackageFlag.Aur
                      }
                    , Package::{ name = "acpi_call" }
                    , Package::{ name = "acpi_call-lts" }
                    ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Tree =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "tree" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Udisks2 =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "udisks2" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Unzip =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "unzip" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Upower =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "upower" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , W3m =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "w3m" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Wal =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "python-pywal" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Xdg =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "xdg-user-dirs" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          , Xsel =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "xsel" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , YoutubeDl =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present = [ Package::{ name = "youtube-dl" } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
          , Zoom =
              merge
                { Pacman = PackageGroup/Metadata::{
                  , present =
                    [ Package::{ name = "zoom", flag = Some PackageFlag.Aur } ]
                  }
                , Dnf = PackageGroup/Metadata::{=}
                , Apt = PackageGroup/Metadata::{=}
                }
                env.package_manager
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
                env.package_manager
          }
          packageGroup

in  toMetadata
