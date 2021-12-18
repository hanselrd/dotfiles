let External/Prelude = ../External/Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let Role/Config = ./Config/Record.partial.dhall

let Role/equal = ../../codegen/Lib/Role/equal.partial.dhall

let env = ../../codegen/environment.partial.dhall

let isEnabled
    : Role -> Bool
    = \(role : Role) ->
        External/Prelude.Bool.or
          ( External/Prelude.List.filterMap
              Role/Config.Type
              Bool
              ( \(roleConfig : Role/Config.Type) ->
                  if    Role/equal roleConfig.role role
                  then  Some roleConfig.enabled
                  else  None Bool
              )
              env.roles
          )

in  isEnabled
