let Theme = ./Enum.partial.dhall

let Theme/Palette = ./Palette/Record.partial.dhall

let Theme/Metadata = ./Metadata/Record.partial.dhall

let toMetadata
    : Theme -> Theme/Metadata.Type
    = \(theme : Theme) ->
        merge
          { Gruvbox = Theme/Metadata::{
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
          , Nord = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#2E3440"
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
              , brightBlack = "#282C34"
              , brightRed = "#E06C75"
              , brightGreen = "#98C379"
              , brightYellow = "#E5C07B"
              , brightBlue = "#61AFEF"
              , brightMagenta = "#C678DD"
              , brightCyan = "#56B6C2"
              , brightWhite = "#ABB2BF"
              }
            }
          , ChallengerDeep = Theme/Metadata::{
            , palette = Theme/Palette::{
              , background = "#1B182C"
              , foreground = "#CBE3E7"
              , cursor = "#FBFCFC"
              , black = "#100E23"
              , red = "#FF8080"
              , green = "#95FFA4"
              , yellow = "#FFE9AA"
              , blue = "#91DDFF"
              , magenta = "#C991E1"
              , cyan = "#AAFFE4"
              , white = "#CBE3E7"
              , brightBlack = "#565575"
              , brightRed = "#FF5458"
              , brightGreen = "#62D196"
              , brightYellow = "#FFB378"
              , brightBlue = "#65B2FF"
              , brightMagenta = "#906CFF"
              , brightCyan = "#63F2F1"
              , brightWhite = "#A6B3CC"
              }
            }
          , Wal = Theme/Metadata::{
            , palette = ~/.cache/wal/colors.dhall ? Theme/Palette::{=}
            }
          }
          theme

in  toMetadata
