let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let sort
    : forall (a : Type) -> List (EnumMeta a).Type -> List a -> List a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(xs : List a) ->
        let maxSort =
              External/Prelude.Natural.listMax
                ( External/Prelude.List.unpackOptionals
                    Natural
                    ( External/Prelude.List.map
                        (EnumMeta a).Type
                        (Optional Natural)
                        (\(enumMeta : (EnumMeta a).Type) -> enumMeta.sort)
                        enumMetas
                    )
                )

        let indices =
              External/Prelude.Natural.enumerate
                (merge { Some = \(n : Natural) -> n + 1, None = 0 } maxSort)

        in  External/Prelude.List.fold
              Natural
              indices
              (List a)
              ( \(index : Natural) ->
                \(acc : List a) ->
                    External/Prelude.List.map
                      (EnumMeta a).Type
                      a
                      (\(enumMeta : (EnumMeta a).Type) -> enumMeta.value)
                      ( External/Prelude.List.filter
                          (EnumMeta a).Type
                          ( \(enumMeta : (EnumMeta a).Type) ->
                              merge
                                { Some =
                                    \(n : Natural) ->
                                      External/Prelude.Natural.equal n index
                                , None = False
                                }
                                enumMeta.sort
                          )
                          enumMetas
                      )
                  # acc
              )
              ( External/Prelude.List.map
                  (EnumMeta a).Type
                  a
                  (\(enumMeta : (EnumMeta a).Type) -> enumMeta.value)
                  ( External/Prelude.List.filter
                      (EnumMeta a).Type
                      ( \(enumMeta : (EnumMeta a).Type) ->
                          External/Prelude.Optional.null Natural enumMeta.sort
                      )
                      enumMetas
                  )
              )

in  sort
