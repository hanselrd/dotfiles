let Configuration = ./Enum.partial.dhall

let toText
    : Configuration -> Text
    = \(configuration : Configuration) ->
        merge
          { Desktop = "desktop", Laptop = "laptop", Remote = "remote" }
          configuration

in  toText
