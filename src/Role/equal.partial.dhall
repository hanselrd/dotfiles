let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/package.dhall
        sha256:46c48bba5eee7807a872bbf6c3cb6ee6c2ec9498de3543c5dcc7dd950e43999d

let Role = ../Role.partial.dhall

let Role/toNatural = ./toNatural.partial.dhall

in  \(a : Role) ->
    \(b : Role) ->
      Prelude.Natural.equal (Role/toNatural a) (Role/toNatural b)
