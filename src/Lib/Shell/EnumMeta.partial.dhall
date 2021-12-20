let External/Prelude = ../External/Prelude.partial.dhall

let Shell = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Default = False, Zsh = False }

let meta =
      { Default = (EnumMeta Shell)::{
        , value = Shell.Default
        , equal =
            \(shell : Shell) -> merge (default // { Default = True }) shell
        }
      , Zsh = (EnumMeta Shell)::{
        , value = Shell.Zsh
        , equal = \(shell : Shell) -> merge (default // { Zsh = True }) shell
        }
      }

let validate = assert : merge meta Shell.Default === meta.Default

in  External/Prelude.Map.values Text (EnumMeta Shell).Type (toMap meta)
