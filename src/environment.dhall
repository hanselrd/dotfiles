''
let Role/Enum = ../src/Lib/Role/Enum.partial.dhall

let Role/Config = ../src/Lib/Role/Config/Record.partial.dhall

in  { Type =
        { user : Text
        , user_name : Text
        , user_email : Text
        , user_home_dir : Text
        , user_cache_dir : Text
        , user_config_dir : Text
        , user_root_dir : Text
        , user_temp_dir : Text
        , configuration : Text
        , roles : List Role/Config.Type
        }
    , default =
      { user = ${Text/show env:DOTFILES_USER as Text}
      , user_name = ${Text/show env:DOTFILES_USER_NAME as Text}
      , user_email = ${Text/show env:DOTFILES_USER_EMAIL as Text}
      , user_home_dir = ${Text/show env:DOTFILES_USER_HOME_DIR as Text}
      , user_cache_dir = ${Text/show env:DOTFILES_USER_CACHE_DIR as Text}
      , user_config_dir = ${Text/show env:DOTFILES_USER_CONFIG_DIR as Text}
      , user_root_dir = ${Text/show env:DOTFILES_USER_ROOT_DIR as Text}
      , user_temp_dir = ${Text/show env:DOTFILES_USER_TEMP_DIR as Text}
      , configuration = ${Text/show env:DOTFILES_CONFIGURATION as Text}
      , roles =
        [ Role/Config::{ role = Role/Enum.Alacritty, enabled = ${env:DOTFILES_ROLE_ALACRITTY as Text} }
        , Role/Config::{ role = Role/Enum.Backgrounds, enabled = ${env:DOTFILES_ROLE_BACKGROUNDS as Text} }
        , Role/Config::{ role = Role/Enum.Bin, enabled = ${env:DOTFILES_ROLE_BIN as Text} }
        , Role/Config::{ role = Role/Enum.Bspwm, enabled = ${env:DOTFILES_ROLE_BSPWM as Text} }
        , Role/Config::{ role = Role/Enum.Ccache, enabled = ${env:DOTFILES_ROLE_CCACHE as Text} }
        , Role/Config::{ role = Role/Enum.Chsh, enabled = ${env:DOTFILES_ROLE_CHSH as Text} }
        , Role/Config::{ role = Role/Enum.Dwm, enabled = ${env:DOTFILES_ROLE_DWM as Text} }
        , Role/Config::{ role = Role/Enum.Elm, enabled = ${env:DOTFILES_ROLE_ELM as Text} }
        , Role/Config::{ role = Role/Enum.Fonts, enabled = ${env:DOTFILES_ROLE_FONTS as Text} }
        , Role/Config::{ role = Role/Enum.Gdb, enabled = ${env:DOTFILES_ROLE_GDB as Text} }
        , Role/Config::{ role = Role/Enum.Git, enabled = ${env:DOTFILES_ROLE_GIT as Text} }
        , Role/Config::{ role = Role/Enum.Gtk, enabled = ${env:DOTFILES_ROLE_GTK as Text} }
        , Role/Config::{ role = Role/Enum.Haskell, enabled = ${env:DOTFILES_ROLE_HASKELL as Text} }
        , Role/Config::{ role = Role/Enum.I3, enabled = ${env:DOTFILES_ROLE_I3 as Text} }
        , Role/Config::{ role = Role/Enum.I3status, enabled = ${env:DOTFILES_ROLE_I3STATUS as Text} }
        , Role/Config::{ role = Role/Enum.Nodejs, enabled = ${env:DOTFILES_ROLE_NODEJS as Text} }
        , Role/Config::{ role = Role/Enum.Packages, enabled = ${env:DOTFILES_ROLE_PACKAGES as Text} }
        , Role/Config::{ role = Role/Enum.Picom, enabled = ${env:DOTFILES_ROLE_PICOM as Text} }
        , Role/Config::{ role = Role/Enum.Python, enabled = ${env:DOTFILES_ROLE_PYTHON as Text} }
        , Role/Config::{ role = Role/Enum.Ranger, enabled = ${env:DOTFILES_ROLE_RANGER as Text} }
        , Role/Config::{ role = Role/Enum.Rofi, enabled = ${env:DOTFILES_ROLE_ROFI as Text} }
        , Role/Config::{ role = Role/Enum.Runit, enabled = ${env:DOTFILES_ROLE_RUNIT as Text} }
        , Role/Config::{ role = Role/Enum.Rust, enabled = ${env:DOTFILES_ROLE_RUST as Text} }
        , Role/Config::{ role = Role/Enum.Ssh, enabled = ${env:DOTFILES_ROLE_SSH as Text} }
        , Role/Config::{ role = Role/Enum.Sxhkd, enabled = ${env:DOTFILES_ROLE_SXHKD as Text} }
        , Role/Config::{ role = Role/Enum.Systemd, enabled = ${env:DOTFILES_ROLE_SYSTEMD as Text} }
        , Role/Config::{ role = Role/Enum.Theme, enabled = ${env:DOTFILES_ROLE_THEME as Text} }
        , Role/Config::{ role = Role/Enum.Tmux, enabled = ${env:DOTFILES_ROLE_TMUX as Text} }
        , Role/Config::{ role = Role/Enum.Urxvt, enabled = ${env:DOTFILES_ROLE_URXVT as Text} }
        , Role/Config::{ role = Role/Enum.Vim, enabled = ${env:DOTFILES_ROLE_VIM as Text} }
        , Role/Config::{ role = Role/Enum.Vscode, enabled = ${env:DOTFILES_ROLE_VSCODE as Text} }
        , Role/Config::{ role = Role/Enum.Xinit, enabled = ${env:DOTFILES_ROLE_XINIT as Text} }
        , Role/Config::{ role = Role/Enum.Xrandr, enabled = ${env:DOTFILES_ROLE_XRANDR as Text} }
        , Role/Config::{ role = Role/Enum.Xrdb, enabled = ${env:DOTFILES_ROLE_XRDB as Text} }
        , Role/Config::{ role = Role/Enum.Zsh, enabled = ${env:DOTFILES_ROLE_ZSH as Text} }
        ]
      }
    }::{=}
''
