let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Theme/toMetadata = ../../../Lib/Theme/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let themeMetadata = Theme/toMetadata env.theme

let Theme/Color/toText =
      ../../../Lib/Theme/Color/toText.partial.dhall themeMetadata.palette

let sanitizeColor =
      \(color : Text) -> External/Prelude.Text.replace "#" "" color ++ "FF"

in  ''
    #!/usr/bin/env sh

    cd "$(dirname "$0")" || exit

    lock() {
        i3lock -c ${sanitizeColor
                      ( Theme/Color/toText
                          themeMetadata.windowManager.clientBackground
                      )} \
        --clock \
        --radius=200 \
        --ring-width=3 \
        --inside-color=${sanitizeColor
                           ( Theme/Color/toText
                               themeMetadata.windowManager.clientBackground
                           )} \
        --ring-color=${sanitizeColor
                         ( Theme/Color/toText
                             themeMetadata.windowManager.clientFocused.text
                         )} \
        --insidever-color=${sanitizeColor
                              ( Theme/Color/toText
                                  themeMetadata.windowManager.clientBackground
                              )} \
        --ringver-color=${sanitizeColor
                            ( Theme/Color/toText
                                themeMetadata.windowManager.clientFocused.border
                            )} \
        --insidewrong-color=${sanitizeColor
                                ( Theme/Color/toText
                                    themeMetadata.windowManager.clientBackground
                                )} \
        --ringwrong-color=${sanitizeColor
                              ( Theme/Color/toText
                                  themeMetadata.windowManager.clientUrgent.border
                              )} \
        --line-uses-inside \
        --keyhl-color=${sanitizeColor
                          ( Theme/Color/toText
                              themeMetadata.windowManager.clientFocused.border
                          )} \
        --bshl-color=${sanitizeColor
                         ( Theme/Color/toText
                             themeMetadata.windowManager.clientUrgent.border
                         )} \
        --separator-color=${sanitizeColor
                              ( Theme/Color/toText
                                  themeMetadata.windowManager.clientFocused.text
                              )} \
        --verif-color=${sanitizeColor
                          ( Theme/Color/toText
                              themeMetadata.windowManager.clientFocused.border
                          )} \
        --wrong-color=${sanitizeColor
                          ( Theme/Color/toText
                              themeMetadata.windowManager.clientUrgent.border
                          )} \
        --layout-color=${sanitizeColor
                           ( Theme/Color/toText
                               themeMetadata.windowManager.clientFocused.text
                           )} \
        --time-color=${sanitizeColor
                         ( Theme/Color/toText
                             themeMetadata.windowManager.clientFocused.text
                         )} \
        --date-color=${sanitizeColor
                         ( Theme/Color/toText
                             themeMetadata.windowManager.clientFocused.text
                         )} \
        --greeter-color=${sanitizeColor
                            ( Theme/Color/toText
                                themeMetadata.windowManager.clientFocused.text
                            )} \
        --time-str="%T%z" \
        --date-str="%Y-%m-%d" \
        --verif-text="" \
        --wrong-text="" \
        --noinput-text="" \
        --lock-text="" \
        --lockfailed-text=""
        # i3lock -c 000000ff --clock --radius=200 --ring-width=3 --inside-color=000000ff --ring-color=ffffffff --insidever-color=000000ff --ringver-color=3d9970ff --insidewrong-color=000000ff --ringwrong-color=ff4136ff --line-uses-inside --keyhl-color=3d9970ff --bshl-color=ff4136ff --separator-color=ffffffff --verif-color=3d9970ff --wrong-color=ff4136ff --layout-color=ffffffff --time-color=ffffffff --date-color=ffffffff --greeter-color=ffffffff --time-str="%T%z" --date-str="%Y-%m-%d" --verif-text="" --wrong-text="" --noinput-text="" --lock-text="" --lockfailed-text=""
        # betterlockscreen -t "Type password to unlock" -l dimblur
    }

    # $1 = {{ inventory_hostname }}
    case "$1" in
        desktop|laptop)
            case "$2" in
                lock)
                    lock
                    ;;
                exit)
                    killall -q i3 dwm bspwm
                    ;;
                suspend)
                    lock &
                    systemctl suspend
                    ;;
                hibernate)
                    lock &
                    systemctl hibernate
                    ;;
                reboot)
                    systemctl reboot
                    ;;
                shutdown)
                    systemctl poweroff
                    ;;
                *)
                    echo "Usage: $0 {{ inventory_hostname }} {lock|exit|suspend|hibernate|reboot|shutdown}"
                    echo "$2 not supported"
                    exit 2
                    ;;
            esac
            ;;
        *)
            echo "Usage: $0 {{ inventory_hostname }} {lock|exit|suspend|hibernate|reboot|shutdown}"
            echo "$1 not supported"
            exit 1
            ;;
    esac
    ''
