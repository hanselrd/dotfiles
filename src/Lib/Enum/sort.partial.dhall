let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let Enum/equal = ../Enum/equal.partial.dhall

let sort
    : forall (a : Type) -> List (EnumMeta a).Type -> List a -> List a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(xs : List a) ->
        let enumMetas =
              External/Prelude.List.concatMap
                a
                (EnumMeta a).Type
                ( \(value : a) ->
                    External/Prelude.List.filter
                      (EnumMeta a).Type
                      ( \(enumMeta : (EnumMeta a).Type) ->
                          Enum/equal a enumMetas enumMeta.value value
                      )
                      enumMetas
                )
                xs

        let Accumulator =
              { sorted : List (EnumMeta a).Type, rest : List (EnumMeta a).Type }

        let partitionMinima =
              \(enumMetas : List (EnumMeta a).Type) ->
                merge
                  { Some =
                      \(n : Natural) ->
                        External/Prelude.List.partition
                          (EnumMeta a).Type
                          ( \(enumMeta : (EnumMeta a).Type) ->
                              External/Prelude.Optional.any
                                Natural
                                (External/Prelude.Natural.greaterThanEqual n)
                                enumMeta.sort
                          )
                          enumMetas
                  , None =
                    { true = [] : List (EnumMeta a).Type, false = enumMetas }
                  }
                  ( External/Prelude.Natural.listMin
                      ( External/Prelude.List.filterMap
                          (EnumMeta a).Type
                          Natural
                          (\(enumMeta : (EnumMeta a).Type) -> enumMeta.sort)
                          enumMetas
                      )
                  )

        let step =
              \(acc : Accumulator) ->
                let p = partitionMinima acc.rest

                in  { sorted = acc.sorted # p.true, rest = p.false }

        in  External/Prelude.List.map
              (EnumMeta a).Type
              a
              (\(enumMeta : (EnumMeta a).Type) -> enumMeta.value)
              ( let acc =
                      External/Prelude.List.fold
                        Natural
                        ( External/Prelude.Natural.enumerate
                            ( External/Prelude.Optional.default
                                Natural
                                0
                                ( External/Prelude.Optional.map
                                    Natural
                                    Natural
                                    (External/Prelude.Operator.`+` 1)
                                    ( External/Prelude.Natural.listMax
                                        ( External/Prelude.List.filterMap
                                            (EnumMeta a).Type
                                            Natural
                                            ( \(enumMeta : (EnumMeta a).Type) ->
                                                enumMeta.sort
                                            )
                                            enumMetas
                                        )
                                    )
                                )
                            )
                        )
                        Accumulator
                        (\(_ : Natural) -> \(acc : Accumulator) -> step acc)
                        { sorted = [] : List (EnumMeta a).Type
                        , rest = enumMetas
                        }

                in  acc.sorted # acc.rest
              )

in  sort
