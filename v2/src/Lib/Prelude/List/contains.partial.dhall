let External/Prelude = ../../External/Prelude.partial.dhall

let contains
    : forall (a : Type) -> a -> (a -> a -> Bool) -> List a -> Bool
    = \(a : Type) ->
      \(needle : a) ->
      \(predicate : a -> a -> Bool) ->
      \(haystack : List a) ->
        External/Prelude.Bool.not
          ( External/Prelude.List.null
              a
              (External/Prelude.List.filter a (predicate needle) haystack)
          )

in  contains
