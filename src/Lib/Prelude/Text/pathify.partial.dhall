let External/Prelude = ../../External/Prelude.partial.dhall

let Prelude/Text/replaces = ./replaces.partial.dhall

let pathify
    : Text -> Text
    = \(path : Text) ->
        Prelude/Text/replaces
          [ External/Prelude.Map.keyText "/////" "/"
          , External/Prelude.Map.keyText "////" "/"
          , External/Prelude.Map.keyText "///" "/"
          , External/Prelude.Map.keyText "//" "/"
          ]
          path

in  pathify
