let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

in  External/Prelude.List.concat
      External/Ansible.Task.Type
      [ [ External/Ansible.Task::{
          , name = Some "Install packages"
          , package = Some External/Ansible.Package::{
            , name = "{{ item }}"
            , state = "present"
            }
          , loop = Some "{{ packages }}"
          , vars = Some
              ( External/Ansible.Vars.object
                  ( toMap
                      { packages =
                          External/Ansible.Vars.array
                            ( External/Prelude.List.map
                                Text
                                External/Ansible.Vars.Type
                                External/Ansible.Vars.string
                                [ "alacritty"
                                , "alsa-utils"
                                , "android-tools"
                                , "arandr"
                                , "archinstall"
                                , "archiso"
                                , "ccache"
                                , "clang"
                                , "cmake"
                                , "ctags"
                                , "cups"
                                , "cups-pdf"
                                , "dbeaver"
                                , "dhall"
                                , "dhall-lsp-server"
                                , "dhall-json"
                                , "dhall-yaml"
                                , "feh"
                                , "ffmpeg"
                                , "flameshot"
                                , "gdb"
                                , "glfw-x11"
                                , "graphviz"
                                , "gutenprint"
                                , "htop"
                                , "i3-gaps"
                                , "i3status-rust"
                                , "libreoffice-fresh"
                                , "lldb"
                                , "lxappearance"
                                , "mesa-demos"
                                , "nasm"
                                , "neofetch"
                                , "ninja"
                                , "nmap"
                                , "noto-fonts"
                                , "noto-fonts-cjk"
                                , "noto-fonts-emoji"
                                , "noto-fonts-extra"
                                , "openresolv"
                                , "openssh"
                                , "openvpn"
                                , "pacman-contrib"
                                , "pandoc"
                                , "picom"
                                , "postgresql"
                                , "pulseaudio"
                                , "python-pip"
                                , "python-pywal"
                                , "python2"
                                , "ranger"
                                , "redshift"
                                , "reflector"
                                , "rofi"
                                , "rpm-tools"
                                , "rsync"
                                , "rxvt-unicode"
                                , "sdl2"
                                , "sdl2_image"
                                , "sdl2_mixer"
                                , "sdl2_net"
                                , "sdl2_ttf"
                                , "shellcheck"
                                , "sshfs"
                                , "strace"
                                , "sxhkd"
                                , "tmux"
                                , "tree"
                                , "ttf-liberation"
                                , "udisks2"
                                , "unzip"
                                , "upower"
                                , "vulkan-intel"
                                , "vulkan-tools"
                                , "w3m"
                                , "xdg-user-dirs"
                                , "xorg"
                                , "xorg-apps"
                                , "xorg-drivers"
                                , "xorg-fonts"
                                , "xorg-xinit"
                                , "xsel"
                                , "youtube-dl"
                                , "zsh"
                                ]
                            )
                      }
                  )
              )
          }
        ]
      , [ External/Ansible.Task::{
          , name = Some "Remove packages"
          , package = Some External/Ansible.Package::{
            , name = "{{ item }}"
            , state = "absent"
            }
          , loop = Some "{{ packages }}"
          , vars = Some
              ( External/Ansible.Vars.object
                  ( toMap
                      { packages =
                          External/Ansible.Vars.array
                            ( External/Prelude.List.map
                                Text
                                External/Ansible.Vars.Type
                                External/Ansible.Vars.string
                                [ "xf86-input-synaptics" ]
                            )
                      }
                  )
              )
          }
        ]
      , [ External/Ansible.Task::{
          , name = Some "Check if yay is installed"
          , stat = Some External/Ansible.Stat::{ path = "/usr/bin/yay" }
          , register = Some "st_yay"
          }
        ]
      , External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          ( \(task : External/Ansible.Task.Type) ->
              task // { when = Some "st_yay.stat.exists" }
          )
          (TaskPool/executeCommands [ "yay -S ??? --needed --noconfirm" ])
      , External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          ( \(task : External/Ansible.Task.Type) ->
              task // { when = Some "st_yay.stat.exists" }
          )
          (TaskPool/executeCommands [ "yay -Rns ??? --unneeded --noconfirm" ])
      ]
