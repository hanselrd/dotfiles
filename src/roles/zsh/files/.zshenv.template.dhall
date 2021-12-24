let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    export EDITOR=vim
    export LESS=-R
    export PAGER=less
    ${External/Prelude.Text.default
        ( if    External/Prelude.Bool.not
                  (Configuration/equal env.configuration Configuration.Remote)
          then  Some
                  ''
                  export BROWSER=brave
                  ${if    Role/enabled Role.Alacritty
                    then  ''
                          export TERMINAL=alacritty
                          ''
                    else  ''
                          export TERMINAL=urxvt
                          ''}
                  export VISUAL="$EDITOR"
                  ''
          else  None Text
        )}

    export GIT_ASKPASS=
    export KEYTIMEOUT=1
    export SSH_ASKPASS=

    ${External/Prelude.Text.default
        ( if    External/Prelude.Bool.not
                  (Configuration/equal env.configuration Configuration.Remote)
          then  Some
                  ''
                  export SXHKD_SHELL=/bin/sh
                  ''
          else  None Text
        )}
    ''
