''
let Role = ../src/Role.partial.dhall

let RoleConfig = ../src/Role/Config.partial.dhall

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
        , roles : List RoleConfig.Type
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
        [ RoleConfig::{ role = Role.Alacritty, enabled = ${env:DOTFILES_ROLE_ALACRITTY as Text} }
        , RoleConfig::{ role = Role.Backgrounds, enabled = ${env:DOTFILES_ROLE_BACKGROUNDS as Text} }
        , RoleConfig::{ role = Role.Bin, enabled = ${env:DOTFILES_ROLE_BIN as Text} }
        , RoleConfig::{ role = Role.Bspwm, enabled = ${env:DOTFILES_ROLE_BSPWM as Text} }
        , RoleConfig::{ role = Role.Ccache, enabled = ${env:DOTFILES_ROLE_CCACHE as Text} }
        , RoleConfig::{ role = Role.Chsh, enabled = ${env:DOTFILES_ROLE_CHSH as Text} }
        , RoleConfig::{ role = Role.Dwm, enabled = ${env:DOTFILES_ROLE_DWM as Text} }
        , RoleConfig::{ role = Role.Elm, enabled = ${env:DOTFILES_ROLE_ELM as Text} }
        , RoleConfig::{ role = Role.Fonts, enabled = ${env:DOTFILES_ROLE_FONTS as Text} }
        , RoleConfig::{ role = Role.Gdb, enabled = ${env:DOTFILES_ROLE_GDB as Text} }
        , RoleConfig::{ role = Role.Git, enabled = ${env:DOTFILES_ROLE_GIT as Text} }
        , RoleConfig::{ role = Role.Gtk, enabled = ${env:DOTFILES_ROLE_GTK as Text} }
        , RoleConfig::{ role = Role.Haskell, enabled = ${env:DOTFILES_ROLE_HASKELL as Text} }
        , RoleConfig::{ role = Role.I3, enabled = ${env:DOTFILES_ROLE_I3 as Text} }
        , RoleConfig::{ role = Role.I3status, enabled = ${env:DOTFILES_ROLE_I3STATUS as Text} }
        , RoleConfig::{ role = Role.Nodejs, enabled = ${env:DOTFILES_ROLE_NODEJS as Text} }
        , RoleConfig::{ role = Role.Packages, enabled = ${env:DOTFILES_ROLE_PACKAGES as Text} }
        , RoleConfig::{ role = Role.Picom, enabled = ${env:DOTFILES_ROLE_PICOM as Text} }
        , RoleConfig::{ role = Role.Python, enabled = ${env:DOTFILES_ROLE_PYTHON as Text} }
        , RoleConfig::{ role = Role.Ranger, enabled = ${env:DOTFILES_ROLE_RANGER as Text} }
        , RoleConfig::{ role = Role.Rofi, enabled = ${env:DOTFILES_ROLE_ROFI as Text} }
        , RoleConfig::{ role = Role.Runit, enabled = ${env:DOTFILES_ROLE_RUNIT as Text} }
        , RoleConfig::{ role = Role.Rust, enabled = ${env:DOTFILES_ROLE_RUST as Text} }
        , RoleConfig::{ role = Role.Ssh, enabled = ${env:DOTFILES_ROLE_SSH as Text} }
        , RoleConfig::{ role = Role.Sxhkd, enabled = ${env:DOTFILES_ROLE_SXHKD as Text} }
        , RoleConfig::{ role = Role.Systemd, enabled = ${env:DOTFILES_ROLE_SYSTEMD as Text} }
        , RoleConfig::{ role = Role.Theme, enabled = ${env:DOTFILES_ROLE_THEME as Text} }
        , RoleConfig::{ role = Role.Tmux, enabled = ${env:DOTFILES_ROLE_TMUX as Text} }
        , RoleConfig::{ role = Role.Urxvt, enabled = ${env:DOTFILES_ROLE_URXVT as Text} }
        , RoleConfig::{ role = Role.Vim, enabled = ${env:DOTFILES_ROLE_VIM as Text} }
        , RoleConfig::{ role = Role.Vscode, enabled = ${env:DOTFILES_ROLE_VSCODE as Text} }
        , RoleConfig::{ role = Role.Xinit, enabled = ${env:DOTFILES_ROLE_XINIT as Text} }
        , RoleConfig::{ role = Role.Xrandr, enabled = ${env:DOTFILES_ROLE_XRANDR as Text} }
        , RoleConfig::{ role = Role.Xrdb, enabled = ${env:DOTFILES_ROLE_XRDB as Text} }
        , RoleConfig::{ role = Role.Zsh, enabled = ${env:DOTFILES_ROLE_ZSH as Text} }
        ]
      }
    }
''
