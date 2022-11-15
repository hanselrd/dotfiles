let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let Enum/toNatural = ../Enum/toNatural.partial.dhall

let Enum/fromNatural = ../Enum/fromNatural.partial.dhall

let sort
    : forall (a : Type) -> List (EnumMeta a).Type -> List a -> List a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(xs : List a) ->
        External/Prelude.List.filterMap
          Natural
          a
          (Enum/fromNatural a enumMetas)
          ( External/Prelude.Natural.sort
              ( External/Prelude.List.map
                  a
                  Natural
                  (Enum/toNatural a enumMetas)
                  xs
              )
          )

in  sort
