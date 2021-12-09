let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let toNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> Natural
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(value : a) ->
        merge
          { Some =
              \ ( indexedEnumMeta
                : { index : Natural, value : (EnumMeta a).Type }
                ) ->
                indexedEnumMeta.index
          , None = External/Prelude.List.length (EnumMeta a).Type enumMetas
          }
          ( External/Prelude.List.head
              { index : Natural, value : (EnumMeta a).Type }
              ( External/Prelude.List.filter
                  { index : Natural, value : (EnumMeta a).Type }
                  ( \ ( indexedEnumMeta
                      : { index : Natural, value : (EnumMeta a).Type }
                      ) ->
                      indexedEnumMeta.value.equal value
                  )
                  (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
              )
          )

in  toNatural
