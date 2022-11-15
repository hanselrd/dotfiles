let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

in  ''
    ${External/Prelude.Text.default
        ( if    Role/enabled Role.Urxvt
          then  Some
                  ''
                  #include ${Directory/toText Directory.Urxvt}/rxvt-unicode
                  ''
          else  None Text
        )}
    ''
