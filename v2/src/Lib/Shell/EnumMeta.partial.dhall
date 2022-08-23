let External/Prelude = ../External/Prelude.partial.dhall

let Shell = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Bash = False, Zsh = False }

let meta =
      { Bash = (EnumMeta Shell)::{
        , value = Shell.Bash
        , text = Some "bash"
        , equal = \(shell : Shell) -> merge (default // { Bash = True }) shell
        }
      , Zsh = (EnumMeta Shell)::{
        , value = Shell.Zsh
        , text = Some "zsh"
        , equal = \(shell : Shell) -> merge (default // { Zsh = True }) shell
        }
      }

let validate = assert : merge meta Shell.Bash === meta.Bash

in  External/Prelude.Map.values Text (EnumMeta Shell).Type (toMap meta)
