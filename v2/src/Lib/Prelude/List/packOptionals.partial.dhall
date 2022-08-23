let External/Prelude = ../../External/Prelude.partial.dhall

let packOptionals
    : forall (a : Type) -> List a -> List (Optional a)
    = \(a : Type) ->
        External/Prelude.List.map a (Optional a) (\(x : a) -> Some x)

in  packOptionals
