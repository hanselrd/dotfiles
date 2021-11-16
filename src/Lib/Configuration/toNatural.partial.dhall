let Configuration = ./Enum.partial.dhall

let toNatural
    : Configuration -> Natural
    = \(configuration : Configuration) ->
        merge { Desktop = 0, Laptop = 1, Remote = 2 } configuration

in  toNatural
