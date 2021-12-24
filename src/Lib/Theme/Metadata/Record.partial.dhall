let Theme/Palette = ../Palette/Record.partial.dhall

let Theme/Color = ../Color/Union.partial.dhall

in  { Type =
        { palette : Theme/Palette.Type
        , windowManager :
            { clientFocused :
                { border : Theme/Color
                , text : Theme/Color
                , indicator : Theme/Color
                }
            , clientFocusedInactive :
                { border : Theme/Color
                , text : Theme/Color
                , indicator : Theme/Color
                }
            , clientUnfocused :
                { border : Theme/Color
                , text : Theme/Color
                , indicator : Theme/Color
                }
            , clientUrgent :
                { border : Theme/Color
                , text : Theme/Color
                , indicator : Theme/Color
                }
            , clientPlaceholder :
                { border : Theme/Color
                , text : Theme/Color
                , indicator : Theme/Color
                }
            , clientBackground : Theme/Color
            }
        , statusBar :
            { background : Theme/Color
            , foreground : Theme/Color
            , separator : Theme/Color
            , focusedWorkspace : { border : Theme/Color, text : Theme/Color }
            , activeWorkspace : { border : Theme/Color, text : Theme/Color }
            , inactiveWorkspace : { border : Theme/Color, text : Theme/Color }
            , urgentWorkspace : { border : Theme/Color, text : Theme/Color }
            , bindingMode : { border : Theme/Color, text : Theme/Color }
            , idle : { background : Theme/Color, foreground : Theme/Color }
            , info : { background : Theme/Color, foreground : Theme/Color }
            , good : { background : Theme/Color, foreground : Theme/Color }
            , warning : { background : Theme/Color, foreground : Theme/Color }
            , critical : { background : Theme/Color, foreground : Theme/Color }
            , alternatingTint :
                { background : Theme/Color, foreground : Theme/Color }
            }
        , launcher :
            { background : Theme/Color
            , foreground : Theme/Color
            , activeBackground : Theme/Color
            , urgentBackground : Theme/Color
            , selectedActiveBackground : Theme/Color
            , selectedNormalBackground : Theme/Color
            , selectedUrgentBackground : Theme/Color
            }
        , terminal :
            { background : Theme/Color
            , foreground : Theme/Color
            , cursor : Theme/Color
            , palette :
                { color0 : Theme/Color
                , color1 : Theme/Color
                , color2 : Theme/Color
                , color3 : Theme/Color
                , color4 : Theme/Color
                , color5 : Theme/Color
                , color6 : Theme/Color
                , color7 : Theme/Color
                , color8 : Theme/Color
                , color9 : Theme/Color
                , color10 : Theme/Color
                , color11 : Theme/Color
                , color12 : Theme/Color
                , color13 : Theme/Color
                , color14 : Theme/Color
                , color15 : Theme/Color
                }
            }
        }
    , default =
      { palette = Theme/Palette::{=}
      , windowManager =
        { clientFocused =
          { border = Theme/Color.Blue
          , text = Theme/Color.BrightWhite
          , indicator = Theme/Color.BrightBlue
          }
        , clientFocusedInactive =
          { border = Theme/Color.Black
          , text = Theme/Color.BrightWhite
          , indicator = Theme/Color.BrightBlack
          }
        , clientUnfocused =
          { border = Theme/Color.Black
          , text = Theme/Color.White
          , indicator = Theme/Color.BrightBlack
          }
        , clientUrgent =
          { border = Theme/Color.Red
          , text = Theme/Color.BrightWhite
          , indicator = Theme/Color.BrightRed
          }
        , clientPlaceholder =
          { border = Theme/Color.Raw "#0C0C0C"
          , text = Theme/Color.BrightWhite
          , indicator = Theme/Color.Raw "#0C0C0C"
          }
        , clientBackground = Theme/Color.Black
        }
      , statusBar =
        { background = Theme/Color.Black
        , foreground = Theme/Color.BrightWhite
        , separator = Theme/Color.Raw "#666666"
        , focusedWorkspace =
          { border = Theme/Color.Blue, text = Theme/Color.BrightWhite }
        , activeWorkspace =
          { border = Theme/Color.Black, text = Theme/Color.BrightWhite }
        , inactiveWorkspace =
          { border = Theme/Color.Black, text = Theme/Color.White }
        , urgentWorkspace =
          { border = Theme/Color.Red, text = Theme/Color.BrightWhite }
        , bindingMode =
          { border = Theme/Color.Magenta, text = Theme/Color.BrightWhite }
        , idle =
          { background = Theme/Color.Black
          , foreground = Theme/Color.BrightWhite
          }
        , info =
          { background = Theme/Color.Blue
          , foreground = Theme/Color.BrightWhite
          }
        , good =
          { background = Theme/Color.Green
          , foreground = Theme/Color.BrightWhite
          }
        , warning =
          { background = Theme/Color.Yellow
          , foreground = Theme/Color.BrightWhite
          }
        , critical =
          { background = Theme/Color.Red, foreground = Theme/Color.BrightWhite }
        , alternatingTint =
          { background = Theme/Color.Raw "#111111"
          , foreground = Theme/Color.Raw "#000000"
          }
        }
      , launcher =
        { background = Theme/Color.Black
        , foreground = Theme/Color.BrightWhite
        , activeBackground = Theme/Color.Blue
        , urgentBackground = Theme/Color.Red
        , selectedActiveBackground = Theme/Color.BrightBlue
        , selectedNormalBackground = Theme/Color.BrightBlack
        , selectedUrgentBackground = Theme/Color.BrightRed
        }
      , terminal =
        { background = Theme/Color.Black
        , foreground = Theme/Color.BrightWhite
        , cursor = Theme/Color.Cursor
        , palette =
          { color0 = Theme/Color.Color0
          , color1 = Theme/Color.Color1
          , color2 = Theme/Color.Color2
          , color3 = Theme/Color.Color3
          , color4 = Theme/Color.Color4
          , color5 = Theme/Color.Color5
          , color6 = Theme/Color.Color6
          , color7 = Theme/Color.Color7
          , color8 = Theme/Color.Color8
          , color9 = Theme/Color.Color9
          , color10 = Theme/Color.Color10
          , color11 = Theme/Color.Color11
          , color12 = Theme/Color.Color12
          , color13 = Theme/Color.Color13
          , color14 = Theme/Color.Color14
          , color15 = Theme/Color.Color15
          }
        }
      }
    }
