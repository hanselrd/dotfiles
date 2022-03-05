let External/Prelude = ../External/Prelude.partial.dhall

let System = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Windows = False, Macos = False, Linux = False }

let meta =
      { Windows = (EnumMeta System)::{
        , value = System.Windows
        , text = Some "Windows"
        , equal =
            \(system : System) -> merge (default // { Windows = True }) system
        }
      , Macos = (EnumMeta System)::{
        , value = System.Macos
        , text = Some "macOS"
        , equal =
            \(system : System) -> merge (default // { Macos = True }) system
        }
      , Linux = (EnumMeta System)::{
        , value = System.Linux
        , text = Some "Linux"
        , equal =
            \(system : System) -> merge (default // { Linux = True }) system
        }
      }

let validate = assert : merge meta System.Windows === meta.Windows

in  External/Prelude.Map.values Text (EnumMeta System).Type (toMap meta)
