let External/Prelude = ../../External/Prelude.partial.dhall

let Theme/Color = ./Union.partial.dhall

let Theme/Palette = ../Palette/Record.partial.dhall

let toText
    : Theme/Palette.Type -> Theme/Color -> Text
    = \(palette : Theme/Palette.Type) ->
      \(color : Theme/Color) ->
        merge
          { Background = palette.background
          , Foreground = palette.foreground
          , Cursor = palette.cursor
          , Black = palette.black
          , Red = palette.red
          , Green = palette.green
          , Yellow = palette.yellow
          , Blue = palette.blue
          , Magenta = palette.magenta
          , Cyan = palette.cyan
          , White = palette.white
          , BrightBlack = palette.brightBlack
          , BrightRed = palette.brightRed
          , BrightGreen = palette.brightGreen
          , BrightYellow = palette.brightYellow
          , BrightBlue = palette.brightBlue
          , BrightMagenta = palette.brightMagenta
          , BrightCyan = palette.brightCyan
          , BrightWhite = palette.brightWhite
          , Color0 = palette.black
          , Color1 = palette.red
          , Color2 = palette.green
          , Color3 = palette.yellow
          , Color4 = palette.blue
          , Color5 = palette.magenta
          , Color6 = palette.cyan
          , Color7 = palette.white
          , Color8 = palette.brightBlack
          , Color9 = palette.brightRed
          , Color10 = palette.brightGreen
          , Color11 = palette.brightYellow
          , Color12 = palette.brightBlue
          , Color13 = palette.brightMagenta
          , Color14 = palette.brightCyan
          , Color15 = palette.brightWhite
          , Raw = External/Prelude.Function.identity Text
          }
          color

in  toText
