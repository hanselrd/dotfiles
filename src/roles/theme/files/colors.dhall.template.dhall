let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Prelude = ../../../Lib/Prelude.partial.dhall

in  ''
    let Theme/Palette = ${Prelude.Text.replaces
                            [ External/Prelude.Map.keyText "{" "{{"
                            , External/Prelude.Map.keyText "}" "}}"
                            ]
                            ../../../Lib/Theme/Palette/Record.partial.dhall as Text}

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
