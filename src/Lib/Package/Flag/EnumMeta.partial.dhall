let External/Prelude = ../../External/Prelude.partial.dhall

let Package/Flag = ./Enum.partial.dhall

let EnumMeta = ../../EnumMeta/Record.partial.dhall

let default = { Aur = False, Repo = False }

let meta =
      { Aur = (EnumMeta Package/Flag)::{
        , value = Package/Flag.Aur
        , text = "aur"
        , equal =
            \(packageFlag : Package/Flag) ->
              merge (default // { Aur = True }) packageFlag
        }
      , Repo = (EnumMeta Package/Flag)::{
        , value = Package/Flag.Repo
        , text = "repo"
        , equal =
            \(packageFlag : Package/Flag) ->
              merge (default // { Repo = True }) packageFlag
        }
      }

let validate = assert : merge meta Package/Flag.Aur === meta.Aur

in  External/Prelude.Map.values Text (EnumMeta Package/Flag).Type (toMap meta)
