let External/Prelude = ../../External/Prelude.partial.dhall

let Role/Config = ./Record.partial.dhall

let Role = ../Enum.partial.dhall

let RoleMeta = ../EnumMeta.partial.dhall

let Enum/equal = ../../Enum/equal.partial.dhall

let Role/sort = ../sort.partial.dhall

let sort
    : List Role/Config.Type -> List Role/Config.Type
    = \(xs : List Role/Config.Type) ->
        let sorted =
              Role/sort
                ( External/Prelude.List.map
                    Role/Config.Type
                    Role
                    (\(roleConfig : Role/Config.Type) -> roleConfig.role)
                    xs
                )

        in  External/Prelude.List.concatMap
              Role
              Role/Config.Type
              ( \(role : Role) ->
                  External/Prelude.List.filter
                    Role/Config.Type
                    ( \(roleConfig : Role/Config.Type) ->
                        Enum/equal Role RoleMeta role roleConfig.role
                    )
                    xs
              )
              sorted

in  sort
