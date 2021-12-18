let External/Prelude = ../../External/Prelude.partial.dhall

let replaces
    : External/Prelude.Map.Type Text Text -> Text -> Text
    = \(needleReplacements : External/Prelude.Map.Type Text Text) ->
      \(haystack : Text) ->
        External/Prelude.List.foldLeft
          (External/Prelude.Map.Entry Text Text)
          needleReplacements
          Text
          ( \(acc : Text) ->
            \(needleReplacement : External/Prelude.Map.Entry Text Text) ->
              External/Prelude.Text.replace
                needleReplacement.mapKey
                needleReplacement.mapValue
                acc
          )
          haystack

in  replaces
