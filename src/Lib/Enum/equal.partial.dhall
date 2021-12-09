let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let equal
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> a -> Bool
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(l : a) ->
      \(r : a) ->
        let predicates =
              External/Prelude.List.map
                (EnumMeta a).Type
                (a -> Bool)
                (\(enumMeta : (EnumMeta a).Type) -> enumMeta.equal)
                enumMetas

        let apply = \(predicate : a -> Bool) -> predicate l && predicate r

        in  External/Prelude.Bool.or
              (External/Prelude.List.map (a -> Bool) Bool apply predicates)

in  equal
