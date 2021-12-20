let External/Prelude = ../External/Prelude.partial.dhall

let TaskPool = ./Alias.partial.dhall

let concat
    : List (Optional TaskPool.Type) -> TaskPool.Type
    = \(xs : List (Optional TaskPool.Type)) ->
        External/Prelude.List.concat
          TaskPool.Entry
          (External/Prelude.List.unpackOptionals TaskPool.Type xs)

in  concat
