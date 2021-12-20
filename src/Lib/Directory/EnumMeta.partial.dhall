let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let Directory = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let env = ../../codegen/environment.partial.dhall

let default = { Background = False }

let meta =
      { Background = (EnumMeta Directory)::{
        , value = Directory.Background
        , text = Some
            ( Prelude.Text.pathify
                "${env.user_root_dir}/usr/local/share/backgrounds"
            )
        , equal =
            \(directory : Directory) ->
              merge (default // { Background = True }) directory
        }
      }

let validate = assert : merge meta Directory.Background === meta.Background

in  External/Prelude.Map.values Text (EnumMeta Directory).Type (toMap meta)
