let Theme = ./Enum.partial.dhall

let Theme/Palette = ./Palette/Record.partial.dhall

let Theme/Color = ./Color/Union.partial.dhall

let Theme/Metadata = ./Metadata/Record.partial.dhall

let toMetadata
    : Theme -> Theme/Metadata.Type
    = \(theme : Theme) ->
        merge
          { ChallengerDeep = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#1E1C31"
              , foreground = "#CBE3E7"
              , cursor = "#FBFCFC"
              , black = "#565575"
              , red = "#FF8080"
              , green = "#95FFA4"
              , yellow = "#FFE9AA"
              , blue = "#91DDFF"
              , magenta = "#C991E1"
              , cyan = "#AAFFE4"
              , white = "#CBE3E7"
              , brightBlack = "#100E23"
              , brightRed = "#FF5458"
              , brightGreen = "#62D196"
              , brightYellow = "#FFB378"
              , brightBlue = "#65B2FF"
              , brightMagenta = "#906CFF"
              , brightCyan = "#63F2F1"
              , brightWhite = "#A6B3CC"
              }
            , statusBar =
              { background = Theme/Color.Background
              , foreground = Theme/Color.Foreground
              , separator = Theme/Color.Raw "#666666"
              , focusedWorkspace =
                { border = Theme/Color.Blue, text = Theme/Color.Background }
              , activeWorkspace =
                { border = Theme/Color.Background
                , text = Theme/Color.Foreground
                }
              , inactiveWorkspace =
                { border = Theme/Color.Background, text = Theme/Color.White }
              , urgentWorkspace =
                { border = Theme/Color.Red, text = Theme/Color.Foreground }
              , bindingMode =
                { border = Theme/Color.Magenta, text = Theme/Color.Foreground }
              , idle =
                { background = Theme/Color.Background
                , foreground = Theme/Color.Foreground
                }
              , info =
                { background = Theme/Color.Blue
                , foreground = Theme/Color.Background
                }
              , good =
                { background = Theme/Color.Green
                , foreground = Theme/Color.Background
                }
              , warning =
                { background = Theme/Color.Yellow
                , foreground = Theme/Color.Background
                }
              , critical =
                { background = Theme/Color.Red
                , foreground = Theme/Color.Background
                }
              , alternatingTint =
                { background = Theme/Color.Raw "#111111"
                , foreground = Theme/Color.Raw "#000000"
                }
              }
            }
          , Gruvbox = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#1D2021"
              , foreground = "#EBDBB2"
              , cursor = "#FBF1C7"
              , black = "#282828"
              , red = "#CC241D"
              , green = "#98971A"
              , yellow = "#D79921"
              , blue = "#458588"
              , magenta = "#B16286"
              , cyan = "#689D6A"
              , white = "#A89984"
              , brightBlack = "#928374"
              , brightRed = "#FB4934"
              , brightGreen = "#B8BB26"
              , brightYellow = "#FABD2F"
              , brightBlue = "#83A598"
              , brightMagenta = "#D3869B"
              , brightCyan = "#8EC07C"
              , brightWhite = "#EBDBB2"
              }
            }
          , Matrix = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#000000"
              , foreground = "#00FF00"
              , cursor = "#00FF00"
              , black = "#001100"
              , red = "#003300"
              , green = "#005500"
              , yellow = "#006600"
              , blue = "#007700"
              , magenta = "#009900"
              , cyan = "#00BB00"
              , white = "#00DD00"
              , brightBlack = "#003300"
              , brightRed = "#005500"
              , brightGreen = "#007700"
              , brightYellow = "#008800"
              , brightBlue = "#009900"
              , brightMagenta = "#00BB00"
              , brightCyan = "#00DD00"
              , brightWhite = "#00FF00"
              }
            }
          , Nord = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#121419"
              , foreground = "#ECEFF4"
              , cursor = "#ECEFF4"
              , black = "#2E3440"
              , red = "#BF616A"
              , green = "#A3BE8C"
              , yellow = "#EBCB8B"
              , blue = "#5E81AC"
              , magenta = "#B48EAD"
              , cyan = "#8FBCBB"
              , white = "#D8DEE9"
              , brightBlack = "#4C566A"
              , brightRed = "#BF616A"
              , brightGreen = "#A3BE8C"
              , brightYellow = "#EBCB8B"
              , brightBlue = "#81A1C1"
              , brightMagenta = "#B48EAD"
              , brightCyan = "#88C0D0"
              , brightWhite = "#ECEFF4"
              }
            }
          , OneDark = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#282C34"
              , foreground = "#ABB2BF"
              , cursor = "#ABB2BF"
              , black = "#282C34"
              , red = "#E06C75"
              , green = "#98C379"
              , yellow = "#E5C07B"
              , blue = "#61AFEF"
              , magenta = "#C678DD"
              , cyan = "#56B6C2"
              , white = "#ABB2BF"
              , brightBlack = "#3A404C"
              , brightRed = "#F77983"
              , brightGreen = "#ADDB8A"
              , brightYellow = "#FCD388"
              , brightBlue = "#68BBFF"
              , brightMagenta = "#DB86F4"
              , brightCyan = "#62CFDB"
              , brightWhite = "#C0C8D6"
              }
            , statusBar =
              { background = Theme/Color.Background
              , foreground = Theme/Color.Foreground
              , separator = Theme/Color.Raw "#666666"
              , focusedWorkspace =
                { border = Theme/Color.Blue, text = Theme/Color.Background }
              , activeWorkspace =
                { border = Theme/Color.Background
                , text = Theme/Color.Foreground
                }
              , inactiveWorkspace =
                { border = Theme/Color.Background, text = Theme/Color.White }
              , urgentWorkspace =
                { border = Theme/Color.Red, text = Theme/Color.Foreground }
              , bindingMode =
                { border = Theme/Color.Magenta, text = Theme/Color.Foreground }
              , idle =
                { background = Theme/Color.Background
                , foreground = Theme/Color.Foreground
                }
              , info =
                { background = Theme/Color.Blue
                , foreground = Theme/Color.Background
                }
              , good =
                { background = Theme/Color.Green
                , foreground = Theme/Color.Background
                }
              , warning =
                { background = Theme/Color.Yellow
                , foreground = Theme/Color.Background
                }
              , critical =
                { background = Theme/Color.Red
                , foreground = Theme/Color.Background
                }
              , alternatingTint =
                { background = Theme/Color.Raw "#111111"
                , foreground = Theme/Color.Raw "#000000"
                }
              }
            }
          , PaperColor = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#121212"
              , foreground = "#C6C6C6"
              , cursor = "#C6C6C6"
              , black = "#1C1C1C"
              , red = "#AF005F"
              , green = "#5FAF00"
              , yellow = "#D7AF5F"
              , blue = "#5FAFD7"
              , magenta = "#808080"
              , cyan = "#D7875F"
              , white = "#D0D0D0"
              , brightBlack = "#585858"
              , brightRed = "#5FAF5F"
              , brightGreen = "#AFD700"
              , brightYellow = "#AF87D7"
              , brightBlue = "#FFAF00"
              , brightMagenta = "#FF5FAF"
              , brightCyan = "#00AFAF"
              , brightWhite = "#5F8787"
              }
            , statusBar =
              { background = Theme/Color.Background
              , foreground = Theme/Color.Foreground
              , separator = Theme/Color.Raw "#666666"
              , focusedWorkspace =
                { border = Theme/Color.Blue, text = Theme/Color.Background }
              , activeWorkspace =
                { border = Theme/Color.Background
                , text = Theme/Color.Foreground
                }
              , inactiveWorkspace =
                { border = Theme/Color.Background, text = Theme/Color.White }
              , urgentWorkspace =
                { border = Theme/Color.Red, text = Theme/Color.Foreground }
              , bindingMode =
                { border = Theme/Color.Magenta, text = Theme/Color.Foreground }
              , idle =
                { background = Theme/Color.Background
                , foreground = Theme/Color.Foreground
                }
              , info =
                { background = Theme/Color.Blue
                , foreground = Theme/Color.Background
                }
              , good =
                { background = Theme/Color.Green
                , foreground = Theme/Color.Background
                }
              , warning =
                { background = Theme/Color.Yellow
                , foreground = Theme/Color.Background
                }
              , critical =
                { background = Theme/Color.Red
                , foreground = Theme/Color.Background
                }
              , alternatingTint =
                { background = Theme/Color.Raw "#111111"
                , foreground = Theme/Color.Raw "#000000"
                }
              }
            }
          , Wal = Theme/Metadata::{
            , palette = ~/.cache/wal/colors.dhall ? Theme/Palette::{=}
            }
          }
          theme

in  toMetadata
