let External/Prelude = ../External/Prelude.partial.dhall

let Prelude = ../Prelude.partial.dhall

let Directory = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let env = ../../codegen/environment.partial.dhall

let default =
      { Alacritty = False, Background = False, Font = False, Theme = False }

let meta =
      { Alacritty = (EnumMeta Directory)::{
        , value = Directory.Alacritty
        , text = Some (Prelude.Text.pathify "${env.user_config_dir}/alacritty")
        , equal =
            \(directory : Directory) ->
              merge (default // { Alacritty = True }) directory
        }
      , Background = (EnumMeta Directory)::{
        , value = Directory.Background
        , text = Some
            ( Prelude.Text.pathify
                "${env.user_root_dir}/usr/local/share/backgrounds"
            )
        , equal =
            \(directory : Directory) ->
              merge (default // { Background = True }) directory
        }
      , Font = (EnumMeta Directory)::{
        , value = Directory.Font
        , text = Some
            (Prelude.Text.pathify "${env.user_root_dir}/usr/local/share/fonts")
        , equal =
            \(directory : Directory) ->
              merge (default // { Font = True }) directory
        }
      , Theme = (EnumMeta Directory)::{
        , value = Directory.Theme
        , text = Some
            (Prelude.Text.pathify "${env.user_config_dir}/wal/templates")
        , equal =
            \(directory : Directory) ->
              merge (default // { Theme = True }) directory
        }
      }

let validate = assert : merge meta Directory.Alacritty === meta.Alacritty

in  External/Prelude.Map.values Text (EnumMeta Directory).Type (toMap meta)
