let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

in  ''
    set preview_images true
    ${External/Prelude.Text.default
        ( if    Role/enabled Role.Urxvt
          then  Some
                  ''
                  set preview_images_method urxvt
                  ''
          else  None Text
        )}
    ''
