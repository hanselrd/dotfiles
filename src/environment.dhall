''
let Role = ../src/Role.partial.dhall

let RoleConfig = ../src/RoleConfig.partial.dhall

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
        [ ${env:DOTFILES_ROLE_ALACRITTY as Text}
        , ${env:DOTFILES_ROLE_BACKGROUNDS as Text}
        , ${env:DOTFILES_ROLE_BIN as Text}
        , ${env:DOTFILES_ROLE_BSPWM as Text}
        , ${env:DOTFILES_ROLE_CCACHE as Text}
        , ${env:DOTFILES_ROLE_CHSH as Text}
        , ${env:DOTFILES_ROLE_DWM as Text}
        , ${env:DOTFILES_ROLE_ELM as Text}
        , ${env:DOTFILES_ROLE_FONTS as Text}
        , ${env:DOTFILES_ROLE_GDB as Text}
        , ${env:DOTFILES_ROLE_GIT as Text}
        , ${env:DOTFILES_ROLE_GTK as Text}
        , ${env:DOTFILES_ROLE_HASKELL as Text}
        , ${env:DOTFILES_ROLE_I3 as Text}
        , ${env:DOTFILES_ROLE_I3STATUS as Text}
        , ${env:DOTFILES_ROLE_NODEJS as Text}
        , ${env:DOTFILES_ROLE_PACKAGES as Text}
        , ${env:DOTFILES_ROLE_PICOM as Text}
        , ${env:DOTFILES_ROLE_PYTHON as Text}
        , ${env:DOTFILES_ROLE_RANGER as Text}
        , ${env:DOTFILES_ROLE_ROFI as Text}
        , ${env:DOTFILES_ROLE_RUNIT as Text}
        , ${env:DOTFILES_ROLE_RUST as Text}
        , ${env:DOTFILES_ROLE_SSH as Text}
        , ${env:DOTFILES_ROLE_SXHKD as Text}
        , ${env:DOTFILES_ROLE_SYSTEMD as Text}
        , ${env:DOTFILES_ROLE_THEME as Text}
        , ${env:DOTFILES_ROLE_TMUX as Text}
        , ${env:DOTFILES_ROLE_URXVT as Text}
        , ${env:DOTFILES_ROLE_VIM as Text}
        , ${env:DOTFILES_ROLE_VSCODE as Text}
        , ${env:DOTFILES_ROLE_XINIT as Text}
        , ${env:DOTFILES_ROLE_XRANDR as Text}
        , ${env:DOTFILES_ROLE_XRDB as Text}
        , ${env:DOTFILES_ROLE_ZSH as Text}
        ]
      }
    }
''
