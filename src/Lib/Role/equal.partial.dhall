let External/Prelude = ../External/Prelude.partial.dhall

let Role = ./Enum.partial.dhall

let Role/toNatural = ./toNatural.partial.dhall

let equal
    : Role -> Role -> Bool
    = \(a : Role) ->
      \(b : Role) ->
        External/Prelude.Natural.equal (Role/toNatural a) (Role/toNatural b)

in  equal
