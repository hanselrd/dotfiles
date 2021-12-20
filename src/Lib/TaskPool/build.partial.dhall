let External/Prelude = ../External/Prelude.partial.dhall

let TaskPool = ./Alias.partial.dhall

let build
    : List (Optional TaskPool.Entry) -> TaskPool.Type
    = External/Prelude.List.unpackOptionals TaskPool.Entry

in  build
