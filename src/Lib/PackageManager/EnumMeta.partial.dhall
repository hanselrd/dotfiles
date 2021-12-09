let External/Prelude = ../External/Prelude.partial.dhall

let PackageManager = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Pacman = False, Dnf = False, Apt = False }

let meta =
      { Pacman = (EnumMeta PackageManager)::{
        , value = PackageManager.Pacman
        , text = "pacman"
        , equal =
            \(packageManager : PackageManager) ->
              merge (default // { Pacman = True }) packageManager
        }
      , Dnf = (EnumMeta PackageManager)::{
        , value = PackageManager.Dnf
        , text = "dnf"
        , equal =
            \(packageManager : PackageManager) ->
              merge (default // { Dnf = True }) packageManager
        }
      , Apt = (EnumMeta PackageManager)::{
        , value = PackageManager.Apt
        , text = "apt"
        , equal =
            \(packageManager : PackageManager) ->
              merge (default // { Apt = True }) packageManager
        }
      }

let validate = assert : merge meta PackageManager.Pacman === meta.Pacman

in  External/Prelude.Map.values Text (EnumMeta PackageManager).Type (toMap meta)
