let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let Enum/equal = ../Enum/equal.partial.dhall

let dedupe
    : forall (a : Type) -> List (EnumMeta a).Type -> List a -> List a
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(xs : List a) ->
        let Accumulator =
              { Type = { processed : List a, unprocessed : List a }
              , default.processed = [] : List a
              }

        in  ( External/Prelude.List.foldLeft
                a
                xs
                Accumulator.Type
                ( \(acc : Accumulator.Type) ->
                  \(x : a) ->
                    let p =
                          External/Prelude.List.partition
                            a
                            (\(x : a) -> Enum/equal a enumMetas x x@1)
                            acc.unprocessed

                    in      acc
                        //  { processed =
                                  acc.processed
                                # External/Prelude.Optional.toList
                                    a
                                    (External/Prelude.List.head a p.true)
                            , unprocessed = p.false
                            }
                )
                Accumulator::{ unprocessed = xs }
            ).processed

in  dedupe
