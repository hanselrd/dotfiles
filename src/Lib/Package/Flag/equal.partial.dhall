let External/Prelude = ../../External/Prelude.partial.dhall

let Package/Flag = ./Enum.partial.dhall

let Package/Flag/toNatural = ./toNatural.partial.dhall

let equal
    : Package/Flag -> Package/Flag -> Bool
    = \(a : Package/Flag) ->
      \(b : Package/Flag) ->
        External/Prelude.Natural.equal
          (Package/Flag/toNatural a)
          (Package/Flag/toNatural b)

in  equal
