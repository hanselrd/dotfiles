let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let Configuration/toText =
      ../../../codegen/Lib/Configuration/toText.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    super + Escape
        killall -q -s SIGUSR1 sxhkd

    super + Return
        $TERMINAL

    super + shift + {Return,e}
        $TERMINAL -e zsh -c "{ranger,$EDITOR}"

    super + shift + z
        $BROWSER

    super + {d,shift + w,shift + s}
        rofi -show {run,window,ssh}

    super + shift + x
        ${Directory/toText
            Directory.Bin}/df_exit "${Configuration/toText
                                        env.configuration}" $(echo -e 'lock\nexit\nsuspend\nhibernate\nreboot\nshutdown' | rofi -dmenu -p "power")

    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  super + alt + {1,2,3}
                      ${Directory/toText
                          Directory.Bin}/df_xrandr "${Configuration/toText
                                                        env.configuration}" {default,dock-only,laptop-dock}
                  ''
          else  None Text
        )}

    @Print
        flameshot gui

    XF86MonBrightness{Down,Up}
        xbacklight -{dec,inc} 5

    XF86Audio{Lower,Raise}Volume
        pactl set-sink-volume @DEFAULT_SINK@ {-,+}5%

    super + XF86Audio{Lower,Raise}Volume
        pactl set-source-volume @DEFAULT_SOURCE@ {-,+}5%

    XF86Audio{_,Mic}Mute
        pactl set-{sink,source}-mute @DEFAULT_{SINK,SOURCE}@ toggle

    F{2,3}
        xbacklight -{dec,inc} 5

    F{7,8}
        pactl set-sink-volume @DEFAULT_SINK@ {-,+}5%

    super + F{7,8}
        pactl set-source-volume @DEFAULT_SOURCE@ {-,+}5%

    F9
        pactl set-sink-mute @DEFAULT_SINK@ toggle

    super + F9
        pactl set-source-mute @DEFAULT_SOURCE@ toggle
    ''
