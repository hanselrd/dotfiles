let External/Prelude = ../External/Prelude.partial.dhall

let Configuration = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Desktop = False, Laptop = False, Remote = False }

let meta =
      { Desktop = (EnumMeta Configuration)::{
        , value = Configuration.Desktop
        , text = "desktop"
        , equal =
            \(configuration : Configuration) ->
              merge (default // { Desktop = True }) configuration
        }
      , Laptop = (EnumMeta Configuration)::{
        , value = Configuration.Laptop
        , text = "laptop"
        , equal =
            \(configuration : Configuration) ->
              merge (default // { Laptop = True }) configuration
        }
      , Remote = (EnumMeta Configuration)::{
        , value = Configuration.Remote
        , text = "remote"
        , equal =
            \(configuration : Configuration) ->
              merge (default // { Remote = True }) configuration
        }
      }

let validate = assert : merge meta Configuration.Desktop === meta.Desktop

in  External/Prelude.Map.values Text (EnumMeta Configuration).Type (toMap meta)
