let External/Prelude = ../External/Prelude.partial.dhall

let Prelude/List/Indexed = ../Prelude/List/Indexed/Record.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let SORT_MULTIPLIER = 10000

let fromNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> Natural -> Optional a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(n : Natural) ->
        External/Prelude.List.head
          a
          ( External/Prelude.List.filterMap
              (Prelude/List/Indexed (EnumMeta a).Type).Type
              a
              ( \ ( indexedEnumMeta
                  : (Prelude/List/Indexed (EnumMeta a).Type).Type
                  ) ->
                  if    External/Prelude.Natural.equal
                          (     External/Prelude.Optional.default
                                  Natural
                                  ( External/Prelude.List.length
                                      (EnumMeta a).Type
                                      enumMetas
                                  )
                                  indexedEnumMeta.value.sort
                              * SORT_MULTIPLIER
                            + indexedEnumMeta.index
                          )
                          n
                  then  Some indexedEnumMeta.value.value
                  else  None a
              )
              (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
          )

in  fromNatural
