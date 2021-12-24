let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Theme/toMetadata = ../../../Lib/Theme/toMetadata.partial.dhall

let Font/toMetadata = ../../../Lib/Font/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let themeMetadata = Theme/toMetadata env.theme

let Theme/Color/toText =
      ../../../Lib/Theme/Color/toText.partial.dhall themeMetadata.palette

let fontMetadata = Font/toMetadata env.font

in  ''
    ! Extensions
    ! ==========
    !
    URxvt*perl-ext-common: default,matcher,selection-to-clipboard,font-size

    ! Fonts/Spacing
    ! =====
    !
    URxvt*font: xft:${fontMetadata.terminal.name} ${External/Prelude.Text.default
                                                      fontMetadata.terminal.style}:size=${External/Prelude.Natural.show
                                                                                            fontMetadata.terminal.size}
    URxvt*allow_bold: true
    URxvt*letterSpace: 0
    URxvt*internalBorder: 15

    ! Scrolling
    ! =========
    !
    URxvt*saveLines: 10000
    URxvt*scrollBar: false
    URxvt*scrollStyle: rxvt
    URxvt*scrollBar_floating: true
    URxvt*scrollColor: ${Theme/Color/toText themeMetadata.terminal.cursor}
    URxvt*scrollBar_right: true
    URxvt*thickness: 12
    URxvt*scrollTtyOutput: false
    URxvt*scrollWithBuffer: true
    URxvt*scrollTtyKeypress: true

    ! Interaction
    ! ===========
    !
    URxvt*cutchars: "\"(),<>[]{}|'`"
    URxvt*url-launcher: xdg-open
    URxvt*matcher.button: 3

    ! Key Bindings
    ! ============
    !
    ! Copy/Paste, same keys as gnome-terminal
    ! https://stackoverflow.com/questions/39884093
    ! Disable ISO 14755 unicode input so we can use Ctrl-Shift bindings
    URxvt*iso14755:        false
    URxvt*iso14755_52:     false
    ! Disable Ctrl-Alt-c & Ctrl-Alt-v bindings (optional)
    URxvt*keysym.C-M-c:    builtin-string:
    URxvt*keysym.C-M-v:    builtin-string:
    ! Bind Ctrl-Shift-c & Ctrl-Shift-v to copy and paste
    ! I dont know why, but I needed to use hex keysym values to get it to work
    URxvt*keysym.C-S-0x43: eval:selection_to_clipboard
    URxvt*keysym.C-S-0x56: eval:paste_clipboard

    ! Theme/Colors
    ! ===========
    !
    URxvt*background: ${Theme/Color/toText themeMetadata.terminal.background}
    URxvt*foreground: ${Theme/Color/toText themeMetadata.terminal.foreground}
    URxvt*cursorColor: ${Theme/Color/toText themeMetadata.terminal.cursor}
    URxvt*cursorBlink: true
    URxvt*fading: 0
    !URxvt*depth: 32
    URxvt*depth: 24
    ! Black + DarkGrey
    URxvt*color0:  ${Theme/Color/toText themeMetadata.terminal.palette.color0}
    URxvt*color8:  ${Theme/Color/toText themeMetadata.terminal.palette.color8}
    ! DarkRed + Red
    URxvt*color1:  ${Theme/Color/toText themeMetadata.terminal.palette.color1}
    URxvt*color9:  ${Theme/Color/toText themeMetadata.terminal.palette.color9}
    ! DarkGreen + Green
    URxvt*color2:  ${Theme/Color/toText themeMetadata.terminal.palette.color2}
    URxvt*color10: ${Theme/Color/toText themeMetadata.terminal.palette.color10}
    ! DarkYellow + Yellow
    URxvt*color3:  ${Theme/Color/toText themeMetadata.terminal.palette.color3}
    URxvt*color11: ${Theme/Color/toText themeMetadata.terminal.palette.color11}
    ! DarkBlue + Blue
    URxvt*color4:  ${Theme/Color/toText themeMetadata.terminal.palette.color4}
    URxvt*color12: ${Theme/Color/toText themeMetadata.terminal.palette.color12}
    ! DarkMagenta + Magenta
    URxvt*color5:  ${Theme/Color/toText themeMetadata.terminal.palette.color5}
    URxvt*color13: ${Theme/Color/toText themeMetadata.terminal.palette.color13}
    ! DarkCyan + Cyan
    URxvt*color6:  ${Theme/Color/toText themeMetadata.terminal.palette.color6}
    URxvt*color14: ${Theme/Color/toText themeMetadata.terminal.palette.color14}
    ! LightGrey + White
    URxvt*color7:  ${Theme/Color/toText themeMetadata.terminal.palette.color7}
    URxvt*color15: ${Theme/Color/toText themeMetadata.terminal.palette.color15}
    ! Black color that will not be affected by bold highlighting
    URxvt*color66: ${Theme/Color/toText themeMetadata.terminal.palette.color0}
    ''
