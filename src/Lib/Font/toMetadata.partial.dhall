let Font = ./Enum.partial.dhall

let Font/Metadata = ./Metadata/Record.partial.dhall

let toMetadata
    : Font -> Font/Metadata.Type
    = \(font : Font) ->
        merge
          { CascadiaCode = Font/Metadata::{
            , windowManager =
              { name = "CaskaydiaCove Nerd Font", style = None Text, size = 10 }
            , statusBar =
              { name = "CaskaydiaCove Nerd Font", style = None Text, size = 10 }
            , launcher =
              { name = "CaskaydiaCove Nerd Font", style = None Text, size = 12 }
            , terminal =
              { name = "CaskaydiaCove Nerd Font Mono"
              , style = None Text
              , size = 9
              }
            , editor =
              { name = "CaskaydiaCove Nerd Font Mono"
              , style = Some "normal"
              , size = 12
              }
            }
          , FantasqueSansMono = Font/Metadata::{
            , windowManager =
              { name = "FantasqueSansMono Nerd Font"
              , style = Some "Bold"
              , size = 10
              }
            , statusBar =
              { name = "FantasqueSansMono Nerd Font"
              , style = Some "Bold"
              , size = 10
              }
            , launcher =
              { name = "FantasqueSansMono Nerd Font"
              , style = Some "Bold"
              , size = 12
              }
            , terminal =
              { name = "FantasqueSansMono Nerd Font Mono"
              , style = Some "Bold"
              , size = 10
              }
            , editor =
              { name = "FantasqueSansMono Nerd Font Mono"
              , style = Some "bold"
              , size = 12
              }
            }
          , Inconsolata = Font/Metadata::{
            , windowManager =
              { name = "Inconsolata Nerd Font", style = Some "Bold", size = 10 }
            , statusBar =
              { name = "Inconsolata Nerd Font", style = Some "Bold", size = 10 }
            , launcher =
              { name = "Inconsolata Nerd Font", style = Some "Bold", size = 12 }
            , terminal =
              { name = "Inconsolata Nerd Font Mono"
              , style = Some "Bold"
              , size = 10
              }
            , editor =
              { name = "Inconsolata Nerd Font Mono"
              , style = Some "bold"
              , size = 12
              }
            }
          , Iosevka = Font/Metadata::{
            , windowManager =
              { name = "Iosevka Nerd Font", style = Some "Heavy", size = 10 }
            , statusBar =
              { name = "Iosevka Nerd Font", style = Some "Heavy", size = 10 }
            , launcher =
              { name = "Iosevka Nerd Font", style = Some "Heavy", size = 12 }
            , terminal =
              { name = "Iosevka Nerd Font Mono"
              , style = Some "Heavy"
              , size = 9
              }
            , editor =
              { name = "Iosevka Nerd Font Mono"
              , style = Some "bold"
              , size = 12
              }
            }
          , JetBrainsMono = Font/Metadata::{
            , windowManager =
              { name = "JetBrainsMono Nerd Font"
              , style = Some "Bold"
              , size = 10
              }
            , statusBar =
              { name = "JetBrainsMono Nerd Font"
              , style = Some "Bold"
              , size = 10
              }
            , launcher =
              { name = "JetBrainsMono Nerd Font"
              , style = Some "Bold"
              , size = 12
              }
            , terminal =
              { name = "JetBrainsMono Nerd Font Mono"
              , style = Some "Bold"
              , size = 9
              }
            , editor =
              { name = "JetBrainsMono Nerd Font Mono"
              , style = Some "bold"
              , size = 12
              }
            }
          }
          font

in  toMetadata
