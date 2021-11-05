''
{ Type =
    { user : Text
    , user_name : Text
    , user_email : Text
    , user_home_dir : Text
    , user_cache_dir : Text
    , user_config_dir : Text
    , user_root_dir : Text
    , user_temp_dir : Text
    , configuration : Text
    , role_alacritty : Bool
    , role_backgrounds : Bool
    , role_bin : Bool
    , role_bspwm : Bool
    , role_ccache : Bool
    , role_chsh : Bool
    , role_dwm : Bool
    , role_elm : Bool
    , role_fonts : Bool
    , role_gdb : Bool
    , role_git : Bool
    , role_gtk : Bool
    , role_haskell : Bool
    , role_i3 : Bool
    , role_i3status : Bool
    , role_nodejs : Bool
    , role_packages : Bool
    , role_picom : Bool
    , role_python : Bool
    , role_ranger : Bool
    , role_rofi : Bool
    , role_runit : Bool
    , role_rust : Bool
    , role_ssh : Bool
    , role_sxhkd : Bool
    , role_systemd : Bool
    , role_theme : Bool
    , role_tmux : Bool
    , role_urxvt : Bool
    , role_vim : Bool
    , role_vscode : Bool
    , role_xinit : Bool
    , role_xrandr : Bool
    , role_xrdb : Bool
    , role_zsh : Bool
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
  , role_alacritty = ${env:DOTFILES_ROLE_ALACRITTY as Text}
  , role_backgrounds  = ${env:DOTFILES_ROLE_BACKGROUNDS as Text}
  , role_bin  = ${env:DOTFILES_ROLE_BIN as Text}
  , role_bspwm  = ${env:DOTFILES_ROLE_BSPWM as Text}
  , role_ccache  = ${env:DOTFILES_ROLE_CCACHE as Text}
  , role_chsh  = ${env:DOTFILES_ROLE_CHSH as Text}
  , role_dwm  = ${env:DOTFILES_ROLE_DWM as Text}
  , role_elm  = ${env:DOTFILES_ROLE_ELM as Text}
  , role_fonts  = ${env:DOTFILES_ROLE_FONTS as Text}
  , role_gdb  = ${env:DOTFILES_ROLE_GDB as Text}
  , role_git  = ${env:DOTFILES_ROLE_GIT as Text}
  , role_gtk  = ${env:DOTFILES_ROLE_GTK as Text}
  , role_haskell  = ${env:DOTFILES_ROLE_HASKELL as Text}
  , role_i3  = ${env:DOTFILES_ROLE_I3 as Text}
  , role_i3status  = ${env:DOTFILES_ROLE_I3STATUS as Text}
  , role_nodejs  = ${env:DOTFILES_ROLE_NODEJS as Text}
  , role_packages  = ${env:DOTFILES_ROLE_PACKAGES as Text}
  , role_picom  = ${env:DOTFILES_ROLE_PICOM as Text}
  , role_python  = ${env:DOTFILES_ROLE_PYTHON as Text}
  , role_ranger  = ${env:DOTFILES_ROLE_RANGER as Text}
  , role_rofi  = ${env:DOTFILES_ROLE_ROFI as Text}
  , role_runit  = ${env:DOTFILES_ROLE_RUNIT as Text}
  , role_rust  = ${env:DOTFILES_ROLE_RUST as Text}
  , role_ssh  = ${env:DOTFILES_ROLE_SSH as Text}
  , role_sxhkd  = ${env:DOTFILES_ROLE_SXHKD as Text}
  , role_systemd  = ${env:DOTFILES_ROLE_SYSTEMD as Text}
  , role_theme  = ${env:DOTFILES_ROLE_THEME as Text}
  , role_tmux  = ${env:DOTFILES_ROLE_TMUX as Text}
  , role_urxvt  = ${env:DOTFILES_ROLE_URXVT as Text}
  , role_vim  = ${env:DOTFILES_ROLE_VIM as Text}
  , role_vscode  = ${env:DOTFILES_ROLE_VSCODE as Text}
  , role_xinit  = ${env:DOTFILES_ROLE_XINIT as Text}
  , role_xrandr  = ${env:DOTFILES_ROLE_XRANDR as Text}
  , role_xrdb  = ${env:DOTFILES_ROLE_XRDB as Text}
  , role_zsh  = ${env:DOTFILES_ROLE_ZSH as Text}
  }
}
''
