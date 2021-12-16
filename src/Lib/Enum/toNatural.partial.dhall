let External/Prelude = ../External/Prelude.partial.dhall

let Prelude/List/Indexed = ../Prelude/List/Indexed/Record.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let toNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> Natural
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(value : a) ->
        merge
          { Some =
              \ ( indexedEnumMeta
                : (Prelude/List/Indexed (EnumMeta a).Type).Type
                ) ->
                indexedEnumMeta.index
          , None = External/Prelude.List.length (EnumMeta a).Type enumMetas
          }
          ( External/Prelude.List.head
              (Prelude/List/Indexed (EnumMeta a).Type).Type
              ( External/Prelude.List.filter
                  (Prelude/List/Indexed (EnumMeta a).Type).Type
                  ( \ ( indexedEnumMeta
                      : (Prelude/List/Indexed (EnumMeta a).Type).Type
                      ) ->
                      indexedEnumMeta.value.equal value
                  )
                  (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
              )
          )

in  toNatural
