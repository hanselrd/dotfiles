let External/Prelude = ../External/Prelude.partial.dhall

let Configuration = ./Enum.partial.dhall

let Configuration/toNatural = ./toNatural.partial.dhall

let equal
    : Configuration -> Configuration -> Bool
    = \(a : Configuration) ->
      \(b : Configuration) ->
        External/Prelude.Natural.equal
          (Configuration/toNatural a)
          (Configuration/toNatural b)

in  equal
