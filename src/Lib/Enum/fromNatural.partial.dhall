let External/Prelude = ../External/Prelude.partial.dhall

let Prelude/List/Indexed = ../Prelude/List/Indexed/Record.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let fromNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> Natural -> Optional a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(n : Natural) ->
        External/Prelude.Optional.map
          (Prelude/List/Indexed (EnumMeta a).Type).Type
          a
          ( \ ( indexedEnumMeta
              : (Prelude/List/Indexed (EnumMeta a).Type).Type
              ) ->
              indexedEnumMeta.value.value
          )
          ( External/Prelude.List.head
              (Prelude/List/Indexed (EnumMeta a).Type).Type
              ( External/Prelude.List.filter
                  (Prelude/List/Indexed (EnumMeta a).Type).Type
                  ( \ ( indexedEnumMeta
                      : (Prelude/List/Indexed (EnumMeta a).Type).Type
                      ) ->
                      External/Prelude.Natural.equal indexedEnumMeta.index n
                  )
                  (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
              )
          )

in  fromNatural
