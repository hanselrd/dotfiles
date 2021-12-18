''
let Theme/Palette = ${env:PWD as Text}/src/Lib/Theme/Palette/Record.partial.dhall

in  Theme/Palette::{{
    , background = "{background}"
    , foreground = "{foreground}"
    , cursor = "{cursor}"
    , black = "{color0}"
    , red = "{color1}"
    , green = "{color2}"
    , yellow = "{color3}"
    , blue = "{color4}"
    , magenta = "{color5}"
    , cyan = "{color6}"
    , white = "{color7}"
    , brightBlack = "{color8}"
    , brightRed = "{color9}"
    , brightGreen = "{color10}"
    , brightYellow = "{color11}"
    , brightBlue = "{color12}"
    , brightMagenta = "{color13}"
    , brightCyan = "{color14}"
    , brightWhite = "{color15}"
    }}
''
