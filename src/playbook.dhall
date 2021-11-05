let Ansible =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-ansible/0.2.2/package.dhall
        sha256:030d7d1b16172afde44843c6e950fcc3382a6653269e36a27ca1d06d75a631ff

let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/package.dhall
        sha256:46c48bba5eee7807a872bbf6c3cb6ee6c2ec9498de3543c5dcc7dd950e43999d

let Role = ./role.partial.dhall

let env = (../build/environment.dhall).default

let roleToText =
      \(role : Role) ->
        merge
          { Alacritty = "alacritty"
          , Backgrounds = "backgrounds"
          , Bin = "bin"
          , Bspwm = "bspwm"
          , Ccache = "ccache"
          , Chsh = "chsh"
          , Dwm = "dwm"
          , Elm = "elm"
          , Fonts = "fonts"
          , Gdb = "gdb"
          , Git = "git"
          , Gtk = "gtk"
          , Haskell = "haskell"
          , I3 = "i3"
          , I3status = "i3status"
          , Nodejs = "nodejs"
          , Packages = "packages"
          , Picom = "picom"
          , Python = "python"
          , Ranger = "ranger"
          , Rofi = "rofi"
          , Runit = "runit"
          , Rust = "rust"
          , Ssh = "ssh"
          , Sxhkd = "sxhkd"
          , Systemd = "systemd"
          , Theme = "theme"
          , Tmux = "tmux"
          , Urxvt = "urxvt"
          , Vim = "vim"
          , Vscode = "vscode"
          , Xinit = "xinit"
          , Xrandr = "xrandr"
          , Xrdb = "xrdb"
          , Zsh = "zsh"
          }
          role

let AddIncludeRoleTaskArgs =
      { Type =
          { role : Role
          , enabled : Bool
          , dependencies : List Bool
          , conflicts : List Bool
          }
      , default = { dependencies = [] : List Bool, conflicts = [] : List Bool }
      }

let addIncludeRoleTask =
      \(args : AddIncludeRoleTaskArgs.Type) ->
        let roleName = roleToText args.role

        in  if        args.enabled
                  &&  Prelude.Bool.and args.dependencies
                  &&  Prelude.Bool.not (Prelude.Bool.or args.conflicts)
            then  Some
                    Ansible.Task::{
                    , name = Some "Run ${roleName} role"
                    , include_role = Some Ansible.IncludeRole::{
                      , name = roleName
                      }
                    }
            else  None Ansible.Task.Type

in  [ Ansible.Play::{
      , hosts = "all"
      , gather_facts = Some True
      , become = Some True
      , tasks = Some
          ( Prelude.List.concat
              Ansible.Task.Type
              [ [ Ansible.Task::{
                  , name = Some "Create user directories"
                  , become = Some True
                  , become_user = Some env.user
                  , file = Some Ansible.File::{
                    , path = "{{ item }}"
                    , state = Some Ansible.File.state.directory
                    }
                  , loop = Some "{{ directories }}"
                  , vars = Some
                      ( Ansible.Vars.object
                          ( toMap
                              { directories =
                                  Ansible.Vars.array
                                    [ Ansible.Vars.string env.user_cache_dir
                                    , Ansible.Vars.string env.user_config_dir
                                    , Ansible.Vars.string env.user_home_dir
                                    , Ansible.Vars.string env.user_root_dir
                                    , Ansible.Vars.string env.user_temp_dir
                                    ]
                              }
                          )
                      )
                  }
                ]
              , Prelude.List.unpackOptionals
                  Ansible.Task.Type
                  [ addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Packages
                      , enabled = env.role_packages
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Theme
                      , enabled = env.role_theme
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Backgrounds
                      , enabled = env.role_backgrounds
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Fonts
                      , enabled = env.role_fonts
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Bin
                      , enabled = env.role_bin
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Systemd
                      , enabled = env.role_systemd
                      , conflicts = [ env.role_runit ]
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Xinit
                      , enabled = env.role_xinit
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Alacritty
                      , enabled = env.role_alacritty
                      , conflicts = [ env.role_urxvt ]
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Urxvt
                      , enabled = env.role_urxvt
                      , conflicts = [ env.role_alacritty ]
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Xrdb
                      , enabled = env.role_xrdb
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Xrandr
                      , enabled = env.role_xrandr
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Gdb
                      , enabled = env.role_gdb
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Rofi
                      , enabled = env.role_rofi
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Sxhkd
                      , enabled = env.role_sxhkd
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.I3
                      , enabled = env.role_i3
                      , dependencies = [ env.role_i3status ]
                      , conflicts = [ env.role_dwm, env.role_bspwm ]
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.I3status
                      , enabled = env.role_i3status
                      , dependencies = [ env.role_i3 ]
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Gtk
                      , enabled = env.role_gtk
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Git
                      , enabled = env.role_git
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Vim
                      , enabled = env.role_vim
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Tmux
                      , enabled = env.role_tmux
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Chsh
                      , enabled = env.role_chsh
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Zsh
                      , enabled = env.role_zsh
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Ssh
                      , enabled = env.role_ssh
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Ccache
                      , enabled = env.role_ccache
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Picom
                      , enabled = env.role_picom
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Ranger
                      , enabled = env.role_ranger
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Vscode
                      , enabled = env.role_vscode
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Nodejs
                      , enabled = env.role_nodejs
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Elm
                      , enabled = env.role_elm
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Rust
                      , enabled = env.role_rust
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Python
                      , enabled = env.role_python
                      }
                  , addIncludeRoleTask
                      AddIncludeRoleTaskArgs::{
                      , role = Role.Haskell
                      , enabled = env.role_haskell
                      }
                  ]
              ]
          )
      }
    ]
