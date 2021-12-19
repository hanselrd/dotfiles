let External/Prelude = ../External/Prelude.partial.dhall

let Font = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default =
      { CascadiaCode = False
      , FantasqueSansMono = False
      , Inconsolata = False
      , Iosevka = False
      , JetBrainsMono = False
      }

let meta =
      { CascadiaCode = (EnumMeta Font)::{
        , value = Font.CascadiaCode
        , text = Some "CascadiaCode.zip"
        , equal =
            \(background : Font) ->
              merge (default // { CascadiaCode = True }) background
        }
      , FantasqueSansMono = (EnumMeta Font)::{
        , value = Font.FantasqueSansMono
        , text = Some "FantasqueSansMono.zip"
        , equal =
            \(background : Font) ->
              merge (default // { FantasqueSansMono = True }) background
        }
      , Inconsolata = (EnumMeta Font)::{
        , value = Font.Inconsolata
        , text = Some "Inconsolata.zip"
        , equal =
            \(background : Font) ->
              merge (default // { Inconsolata = True }) background
        }
      , Iosevka = (EnumMeta Font)::{
        , value = Font.Iosevka
        , text = Some "Iosevka.zip"
        , equal =
            \(background : Font) ->
              merge (default // { Iosevka = True }) background
        }
      , JetBrainsMono = (EnumMeta Font)::{
        , value = Font.JetBrainsMono
        , text = Some "JetBrainsMono.zip"
        , equal =
            \(background : Font) ->
              merge (default // { JetBrainsMono = True }) background
        }
      }

let validate = assert : merge meta Font.CascadiaCode === meta.CascadiaCode

in  External/Prelude.Map.values Text (EnumMeta Font).Type (toMap meta)
