let External/Prelude = ../External/Prelude.partial.dhall

let Theme = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { Gruvbox = False
      , Nord = False
      , OneDark = False
      , ChallengerDeep = False
      , Wal = False
      }

let meta =
      { Gruvbox = (EnumMeta Theme)::{
        , value = Theme.Gruvbox
        , equal =
            \(theme : Theme) -> merge (default // { Gruvbox = True }) theme
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
      , ChallengerDeep = (EnumMeta Theme)::{
        , value = Theme.ChallengerDeep
        , equal =
            \(theme : Theme) ->
              merge (default // { ChallengerDeep = True }) theme
        }
      , Wal = (EnumMeta Theme)::{
        , value = Theme.Wal
        , equal = \(theme : Theme) -> merge (default // { Wal = True }) theme
        }
      }

let validate = assert : merge meta Theme.Gruvbox === meta.Gruvbox

in  External/Prelude.Map.values Text (EnumMeta Theme).Type (toMap meta)
