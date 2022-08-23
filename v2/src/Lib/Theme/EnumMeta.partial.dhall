let External/Prelude = ../External/Prelude.partial.dhall

let Theme = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { ChallengerDeep = False
      , Gruvbox = False
      , Matrix = False
      , Nord = False
      , OneDark = False
      , PaperColor = False
      , Wal = False
      }

let meta =
      { ChallengerDeep = (EnumMeta Theme)::{
        , value = Theme.ChallengerDeep
        , equal =
            \(theme : Theme) ->
              merge (default // { ChallengerDeep = True }) theme
        }
      , Gruvbox = (EnumMeta Theme)::{
        , value = Theme.Gruvbox
        , equal =
            \(theme : Theme) -> merge (default // { Gruvbox = True }) theme
        }
      , Matrix = (EnumMeta Theme)::{
        , value = Theme.Matrix
        , equal = \(theme : Theme) -> merge (default // { Matrix = True }) theme
        }
      , Nord = (EnumMeta Theme)::{
        , value = Theme.Nord
        , equal = \(theme : Theme) -> merge (default // { Nord = True }) theme
        }
      , OneDark = (EnumMeta Theme)::{
        , value = Theme.OneDark
        , equal =
            \(theme : Theme) -> merge (default // { OneDark = True }) theme
        }
      , PaperColor = (EnumMeta Theme)::{
        , value = Theme.PaperColor
        , equal =
            \(theme : Theme) -> merge (default // { PaperColor = True }) theme
        }
      , Wal = (EnumMeta Theme)::{
        , value = Theme.Wal
        , equal = \(theme : Theme) -> merge (default // { Wal = True }) theme
        }
      }

let validate = assert : merge meta Theme.ChallengerDeep === meta.ChallengerDeep

in  External/Prelude.Map.values Text (EnumMeta Theme).Type (toMap meta)
