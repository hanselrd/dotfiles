let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let Theme/toMetadata = ../../../Lib/Theme/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let themeMetadata = Theme/toMetadata env.theme

let Theme/Color/toText =
      ../../../Lib/Theme/Color/toText.partial.dhall themeMetadata.palette

in  ''
    # i3status configuration file.
    # see "man i3status" for documentation.

    # It is important that this file is edited as UTF-8.
    # The following line should contain a sharp s:
    # ß
    # If the above line is not correctly displayed, fix your editor first!

    # general {
    #         colors = true
    #         interval = 5
    # }
    general {
            output_format = "i3bar"
            colors = true
            color_good = "${Theme/Color/toText
                              themeMetadata.statusBar.good.background}"
            color_degraded = "${Theme/Color/toText
                                  themeMetadata.statusBar.warning.background}"
            color_bad = "${Theme/Color/toText
                             themeMetadata.statusBar.critical.background}"
            interval = 5
    }

    #https://apps.timwhitlock.info/emoji/tables/unicode

    #order += "ipv6"
    order += "disk /"
    order += "wireless _first_"
    order += "ethernet _first_"
    order += "path_exists VPN"
    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  order += "battery all"
                  ''
          else  None Text
        )}
    order += "cpu_usage"
    order += "cpu_temperature all"
    order += "load"
    order += "memory"
    order += "volume master"
    order += "tztime local"

    disk "/" {
            # format = "%avail"
            format = "💾%avail"
    }

    wireless _first_ {
            # format_up = "W: %quality %essid %ip (%bitrate)"
            # format_down = "W: OFF"
            format_up = "📶%quality %essid %ip (%bitrate)"
            format_down = "📶OFF"
            format_quality = "%d%s"
    }

    ethernet _first_ {
            # if you use %speed, i3status requires root privileges
            # format_up = "E: %ip (%speed)"
            # format_down = "E: OFF"
            format_up = "📟%ip (%speed)"
            format_down = "📟OFF"
    }

    path_exists VPN {
            # path exists when a VPN tunnel launched by nmcli/nm-applet is active
            # format = "VPN: ON"
            # format_down = "VPN: OFF"
            format = "🔒ON"
            format_down = "🔒OFF"
            path = "/proc/sys/net/ipv4/conf/tun0"
    }

    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  battery all {
                          # format = "%status %percentage %remaining"
                          format = "🔋%status %percentage %remaining"
                          hide_seconds = true
                          integer_battery_capacity = true
                  }
                  ''
          else  None Text
        )}

    cpu_usage {
            # format = "U: %usage"
            format = "💻%usage"
    }

    cpu_temperature all {
            # format = "T: %degrees°C"
            format = "🔥%degrees°C"
    }

    load {
            # format = "L: %1min"
            format = "🔃%1min"
    }

    memory {
            # format = "M: %used/%total"
            format = "📒%used/%total"
            memory_used_method = memavailable
    }

    volume master {
            # format = "V: %volume"
            # format_muted = "V: MUTED (%volume)"
            format = "🔊%volume"
            format_muted = "🔊MUTED (%volume)"
            device = "default"
    }

    tztime local {
           #format = "%Y-%m-%d %H:%M:%S"
           # format = "%a %m/%d/%Y %r"
           format = "📅%a %m/%d/%Y %r⏰"
    }
    ''
