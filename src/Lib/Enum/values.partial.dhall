let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let Enum/sort = ./sort.partial.dhall

let values
    : forall (a : Type) -> List (EnumMeta a).Type -> List a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
        Enum/sort
          a
          enumMetas
          ( External/Prelude.List.map
              (EnumMeta a).Type
              a
              (\(enumMeta : (EnumMeta a).Type) -> enumMeta.value)
              enumMetas
          )

in  values
