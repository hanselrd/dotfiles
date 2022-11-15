let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let Enum/equal = ../Enum/equal.partial.dhall

let SORT_MULTIPLIER = 10000

let toNatural
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> Natural
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(value : a) ->
        External/Prelude.Optional.default
          Natural
          0
          ( External/Prelude.List.head
              Natural
              ( External/Prelude.List.filterMap
                  (Prelude.List.Indexed (EnumMeta a).Type).Type
                  Natural
                  ( \ ( indexedEnumMeta
                      : (Prelude.List.Indexed (EnumMeta a).Type).Type
                      ) ->
                      if    Enum/equal
                              a
                              enumMetas
                              indexedEnumMeta.value.value
                              value
                      then  Some
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
                      else  None Natural
                  )
                  (External/Prelude.List.indexed (EnumMeta a).Type enumMetas)
              )
          )

in  toNatural
