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
            \(font : Font) -> merge (default // { CascadiaCode = True }) font
        }
      , FantasqueSansMono = (EnumMeta Font)::{
        , value = Font.FantasqueSansMono
        , text = Some "FantasqueSansMono.zip"
        , equal =
            \(font : Font) ->
              merge (default // { FantasqueSansMono = True }) font
        }
      , Inconsolata = (EnumMeta Font)::{
        , value = Font.Inconsolata
        , text = Some "Inconsolata.zip"
        , equal =
            \(font : Font) -> merge (default // { Inconsolata = True }) font
        }
      , Iosevka = (EnumMeta Font)::{
        , value = Font.Iosevka
        , skip = True
        , text = Some "Iosevka.zip"
        , equal = \(font : Font) -> merge (default // { Iosevka = True }) font
        }
      , JetBrainsMono = (EnumMeta Font)::{
        , value = Font.JetBrainsMono
        , text = Some "JetBrainsMono.zip"
        , equal =
            \(font : Font) -> merge (default // { JetBrainsMono = True }) font
        }
      }

let validate = assert : merge meta Font.CascadiaCode === meta.CascadiaCode

in  External/Prelude.Map.values Text (EnumMeta Font).Type (toMap meta)
