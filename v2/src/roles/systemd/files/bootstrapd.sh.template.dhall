let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Background/toText = ../../../codegen/Lib/Background/toText.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let Configuration/toText =
      ../../../codegen/Lib/Configuration/toText.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    #!/usr/bin/env sh

    cleanup_handler() {
        pkill -P $$
        exit 0
    }

    trap 'cleanup_handler' INT TERM

    # sxhkd
    sxhkd &

    # nm-applet
    nm-applet &

    # xrandr
    ${Directory/toText
        Directory.Bin}/df_xrandr "${Configuration/toText
                                      env.configuration}" default &

    # picom
    ${Directory/toText Directory.Bin}/df_picom "${Configuration/toText
                                                    env.configuration}" start &

    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  # xinput (enable tab to click)
                  xinput set-prop "$(xinput list | grep -E "Touchpad|TouchPad" | cut -f 2 | cut -d "=" -f 2)" "$(xinput list-props "$(xinput list | grep -E "Touchpad|TouchPad" | cut -f 2 | cut -d "=" -f 2)" | grep "libinput Tapping Enabled (" | cut -d " " -f 4 | cut -d "(" -f 2 | cut -d ")" -f 1)" 1 &
                  ''
          else  None Text
        )}

    # redshift
    ${Directory/toText
        Directory.Bin}/df_redshift "${Configuration/toText
                                        env.configuration}" start &

    # feh
    feh --bg-scale "${Directory/toText
                        Directory.Background}/${Background/toText
                                                  env.background}" &

    # wal
    # TODO: write script to generate wal colorscheme (df_wal?) [wal -b 000000 -i BG -nste]
    #wal -b 000000 -n -i "${Directory/toText
                              Directory.Background}/${Background/toText
                                                        env.background}" &
    #wal -b 000000 --saturate 0.3 -n -i "${Directory/toText
                                             Directory.Background}/${Background/toText
                                                                       env.background}" &

    # betterlockscreen (lockscreen)
    betterlockscreen -u "${Directory/toText
                             Directory.Background}/${Background/toText
                                                       env.background}" &

    # flameshot
    flameshot &
    flameshot config -t false &

    while true; do
        sleep 1
    done
    ''
