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

    @Print
        flameshot gui

    XF86MonBrightness{Up,Down}
        xbacklight -{inc,dec} 5

    XF86Audio{Lower,Raise}Volume
        pactl set-sink-volume @DEFAULT_SINK@ {-,+}5%

    super + XF86Audio{Lower,Raise}Volume
        pactl set-source-volume @DEFAULT_SOURCE@ {-,+}5%

    XF86Audio{_,Mic}Mute
        pactl set-{sink,source}-mute @DEFAULT_{SINK,SOURCE}@ toggle

    F{2,3}
        xbacklight -{inc,dec} 5

    F{7,8}
        pactl set-sink-volume @DEFAULT_SINK@ {-,+}5%

    super + F{7,8}
        pactl set-source-volume @DEFAULT_SOURCE@ {-,+}5%

    F9
        pactl set-sink-mute @DEFAULT_SINK@ toggle

    super + F9
        pactl set-source-mute @DEFAULT_SOURCE@ toggle
    ''
