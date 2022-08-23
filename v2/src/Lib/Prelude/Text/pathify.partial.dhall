let External/Prelude = ../../External/Prelude.partial.dhall

let Text/replaces = ./replaces.partial.dhall

let pathify
    : Text -> Text
    = \(path : Text) ->
        Text/replaces
          [ External/Prelude.Map.keyText "/////" "/"
          , External/Prelude.Map.keyText "////" "/"
          , External/Prelude.Map.keyText "///" "/"
          , External/Prelude.Map.keyText "//" "/"
          ]
          path

in  pathify
