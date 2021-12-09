let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let fromNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> Natural -> Optional a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(n : Natural) ->
        merge
          { Some =
              \ ( indexedEnumMeta
                : { index : Natural, value : (EnumMeta a).Type }
                ) ->
                Some indexedEnumMeta.value.value
          , None = None a
          }
          ( External/Prelude.List.head
              { index : Natural, value : (EnumMeta a).Type }
              ( External/Prelude.List.filter
                  { index : Natural, value : (EnumMeta a).Type }
                  ( \ ( indexedEnumMeta
                      : { index : Natural, value : (EnumMeta a).Type }
                      ) ->
                      External/Prelude.Natural.equal indexedEnumMeta.index n
                  )
                  (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
              )
          )

in  fromNatural
