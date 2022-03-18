let External/Prelude = ./Lib/External/Prelude.partial.dhall

in  ''
    let Background = ../Lib/Background/Enum.partial.dhall

    let Configuration = ../Lib/Configuration/Enum.partial.dhall

    let Font = ../Lib/Font/Enum.partial.dhall

    let PackageManager = ../Lib/PackageManager/Enum.partial.dhall

    let Role = ../Lib/Role/Enum.partial.dhall

    let Role/Config = ../Lib/Role/Config/Record.partial.dhall

    let System = ../Lib/System/Enum.partial.dhall

    let Theme = ../Lib/Theme/Enum.partial.dhall

    in  { Type =
            { user : Text
            , user_name : Text
            , user_email : Text
            , user_home_dir : Text
            , user_cache_dir : Text
            , user_config_dir : Text
            , user_root_dir : Text
            , user_temp_dir : Text
            , system : System
            , configuration : Configuration
            , package_manager : PackageManager
            , background : Background
            , theme : Theme
            , font : Font
            , unsafe_ignore_dependencies : List Role
            , roles : List Role/Config.Type
            }
        , default =
          { user = ${External/Prelude.Text.show env:DOTFILES_USER as Text}
          , user_name = ${External/Prelude.Text.show
                            env:DOTFILES_USER_NAME as Text}
          , user_email = ${External/Prelude.Text.show
                             env:DOTFILES_USER_EMAIL as Text}
          , user_home_dir = ${External/Prelude.Text.show
                                env:DOTFILES_USER_HOME_DIR as Text}
          , user_cache_dir = ${External/Prelude.Text.show
                                 env:DOTFILES_USER_CACHE_DIR as Text}
          , user_config_dir = ${External/Prelude.Text.show
                                  env:DOTFILES_USER_CONFIG_DIR as Text}
          , user_root_dir = ${External/Prelude.Text.show
                                env:DOTFILES_USER_ROOT_DIR as Text}
          , user_temp_dir = ${External/Prelude.Text.show
                                env:DOTFILES_USER_TEMP_DIR as Text}
          , system = System.${env:DOTFILES_SYSTEM as Text}
          , configuration = Configuration.${env:DOTFILES_CONFIGURATION as Text}
          , package_manager = PackageManager.${env:DOTFILES_PACKAGE_MANAGER as Text}
          , background = Background.${env:DOTFILES_BACKGROUND as Text}
          , theme = Theme.${env:DOTFILES_THEME as Text}
          , font = Font.${env:DOTFILES_FONT as Text}
          , unsafe_ignore_dependencies = ${env:DOTFILES_ROLES_UNSAFE_IGNORE_DEPENDENCIES as Text} : List Role
          , roles =
            [ Role/Config::{ role = Role.Alacritty, enabled = ${env:DOTFILES_ROLE_ALACRITTY as Text} }
            , Role/Config::{ role = Role.Alsa, enabled = ${env:DOTFILES_ROLE_ALSA as Text} }
            , Role/Config::{ role = Role.Backgrounds, enabled = ${env:DOTFILES_ROLE_BACKGROUNDS as Text} }
            , Role/Config::{ role = Role.Bin, enabled = ${env:DOTFILES_ROLE_BIN as Text} }
            , Role/Config::{ role = Role.Bspwm, enabled = ${env:DOTFILES_ROLE_BSPWM as Text} }
            , Role/Config::{ role = Role.Ccache, enabled = ${env:DOTFILES_ROLE_CCACHE as Text} }
            , Role/Config::{ role = Role.Chsh, enabled = ${env:DOTFILES_ROLE_CHSH as Text} }
            , Role/Config::{ role = Role.Common, enabled = True }
            , Role/Config::{ role = Role.Docker, enabled = ${env:DOTFILES_ROLE_DOCKER as Text} }
            , Role/Config::{ role = Role.Dwm, enabled = ${env:DOTFILES_ROLE_DWM as Text} }
            , Role/Config::{ role = Role.Elm, enabled = ${env:DOTFILES_ROLE_ELM as Text} }
            , Role/Config::{ role = Role.Fonts, enabled = ${env:DOTFILES_ROLE_FONTS as Text} }
            , Role/Config::{ role = Role.Gdb, enabled = ${env:DOTFILES_ROLE_GDB as Text} }
            , Role/Config::{ role = Role.Git, enabled = ${env:DOTFILES_ROLE_GIT as Text} }
            , Role/Config::{ role = Role.Gtk, enabled = ${env:DOTFILES_ROLE_GTK as Text} }
            , Role/Config::{ role = Role.Haskell, enabled = ${env:DOTFILES_ROLE_HASKELL as Text} }
            , Role/Config::{ role = Role.I3, enabled = ${env:DOTFILES_ROLE_I3 as Text} }
            , Role/Config::{ role = Role.I3status, enabled = ${env:DOTFILES_ROLE_I3STATUS as Text} }
            , Role/Config::{ role = Role.Kernel, enabled = ${env:DOTFILES_ROLE_KERNEL as Text} }
            , Role/Config::{ role = Role.Lua, enabled = ${env:DOTFILES_ROLE_LUA as Text} }
            , Role/Config::{ role = Role.Nodejs, enabled = ${env:DOTFILES_ROLE_NODEJS as Text} }
            , Role/Config::{ role = Role.Packages, enabled = ${env:DOTFILES_ROLE_PACKAGES as Text} }
            , Role/Config::{ role = Role.Picom, enabled = ${env:DOTFILES_ROLE_PICOM as Text} }
            , Role/Config::{ role = Role.Polybar, enabled = ${env:DOTFILES_ROLE_POLYBAR as Text} }
            , Role/Config::{ role = Role.Purescript, enabled = ${env:DOTFILES_ROLE_PURESCRIPT as Text} }
            , Role/Config::{ role = Role.Python, enabled = ${env:DOTFILES_ROLE_PYTHON as Text} }
            , Role/Config::{ role = Role.Ranger, enabled = ${env:DOTFILES_ROLE_RANGER as Text} }
            , Role/Config::{ role = Role.Rofi, enabled = ${env:DOTFILES_ROLE_ROFI as Text} }
            , Role/Config::{ role = Role.Runit, enabled = ${env:DOTFILES_ROLE_RUNIT as Text} }
            , Role/Config::{ role = Role.Rust, enabled = ${env:DOTFILES_ROLE_RUST as Text} }
            , Role/Config::{ role = Role.Ssh, enabled = ${env:DOTFILES_ROLE_SSH as Text} }
            , Role/Config::{ role = Role.Sxhkd, enabled = ${env:DOTFILES_ROLE_SXHKD as Text} }
            , Role/Config::{ role = Role.Systemd, enabled = ${env:DOTFILES_ROLE_SYSTEMD as Text} }
            , Role/Config::{ role = Role.Theme, enabled = ${env:DOTFILES_ROLE_THEME as Text} }
            , Role/Config::{ role = Role.Tlp, enabled = ${env:DOTFILES_ROLE_TLP as Text} }
            , Role/Config::{ role = Role.Tmux, enabled = ${env:DOTFILES_ROLE_TMUX as Text} }
            , Role/Config::{ role = Role.Urxvt, enabled = ${env:DOTFILES_ROLE_URXVT as Text} }
            , Role/Config::{ role = Role.Vim, enabled = ${env:DOTFILES_ROLE_VIM as Text} }
            , Role/Config::{ role = Role.Vscode, enabled = ${env:DOTFILES_ROLE_VSCODE as Text} }
            , Role/Config::{ role = Role.Xinit, enabled = ${env:DOTFILES_ROLE_XINIT as Text} }
            , Role/Config::{ role = Role.Xrandr, enabled = ${env:DOTFILES_ROLE_XRANDR as Text} }
            , Role/Config::{ role = Role.Xrdb, enabled = ${env:DOTFILES_ROLE_XRDB as Text} }
            , Role/Config::{ role = Role.Zsh, enabled = ${env:DOTFILES_ROLE_ZSH as Text} }
            ]
          }
        }::{=}
    ''
