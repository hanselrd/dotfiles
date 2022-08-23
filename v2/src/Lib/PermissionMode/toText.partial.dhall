let External/Prelude = ../External/Prelude.partial.dhall

let PermissionMode = ./Record.partial.dhall

let Permission = ../Permission/Enum.partial.dhall

let Permission/dedupe = ../../codegen/Lib/Permission/dedupe.partial.dhall

let Permission/toText = ../../codegen/Lib/Permission/toText.partial.dhall

let toText
    : PermissionMode.Type -> Text
    = \(permissionMode : PermissionMode.Type) ->
            "u="
        ++  External/Prelude.Text.concatSep
              ""
              ( External/Prelude.List.map
                  Permission
                  Text
                  Permission/toText
                  (Permission/dedupe permissionMode.user)
              )
        ++  ",g="
        ++  External/Prelude.Text.concatSep
              ""
              ( External/Prelude.List.map
                  Permission
                  Text
                  Permission/toText
                  (Permission/dedupe permissionMode.group)
              )
        ++  ",o="
        ++  External/Prelude.Text.concatSep
              ""
              ( External/Prelude.List.map
                  Permission
                  Text
                  Permission/toText
                  (Permission/dedupe permissionMode.other)
              )

in  toText
