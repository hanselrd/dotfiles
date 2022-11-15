let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    if [ -f ${Directory/toText Directory.Xrdb}.Xresources ]; then
        xrdb -merge -I$${env.user_home_dir} ${Directory/toText
                                                Directory.Xrdb}/.Xresources
    fi

    ${External/Prelude.Text.default
        ( if    Role/enabled Role.I3
          then  Some
                  ''
                  exec i3
                  ''
          else  None Text
        )}
    ''
