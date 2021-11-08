let External/Prelude = ../External/Prelude.partial.dhall

let Role/Enum = ./Enum.partial.dhall

let Role/toNatural = ./toNatural.partial.dhall

let equal
    : Role/Enum -> Role/Enum -> Bool
    = \(a : Role/Enum) ->
      \(b : Role/Enum) ->
        External/Prelude.Natural.equal (Role/toNatural a) (Role/toNatural b)

in  equal
