let External/Prelude = ../External/Prelude.partial.dhall

let PackageFlag = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Aur = False, Repo = False }

let meta =
      { Aur = (EnumMeta PackageFlag)::{
        , value = PackageFlag.Aur
        , text = Some "aur"
        , equal =
            \(packageFlag : PackageFlag) ->
              merge (default // { Aur = True }) packageFlag
        }
      , Repo = (EnumMeta PackageFlag)::{
        , value = PackageFlag.Repo
        , text = Some "repo"
        , equal =
            \(packageFlag : PackageFlag) ->
              merge (default // { Repo = True }) packageFlag
        }
      }

let validate = assert : merge meta PackageFlag.Aur === meta.Aur

in  External/Prelude.Map.values Text (EnumMeta PackageFlag).Type (toMap meta)
