let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let PackageManager = ../../../Lib/PackageManager/Enum.partial.dhall

let PackageManager/equal =
      ../../../codegen/Lib/PackageManager/equal.partial.dhall

let Theme/toMetadata = ../../../Lib/Theme/toMetadata.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let themeMetadata = Theme/toMetadata env.theme

let Theme/Color/toText =
      ../../../Lib/Theme/Color/toText.partial.dhall themeMetadata.palette

in  ''
    icons = "awesome"

    [theme]
    name = "modern"
    [theme.overrides]
    idle_bg = "${Theme/Color/toText themeMetadata.statusBar.idle.background}"
    idle_fg = "${Theme/Color/toText themeMetadata.statusBar.idle.foreground}"
    info_bg = "${Theme/Color/toText themeMetadata.statusBar.info.background}"
    info_fg = "${Theme/Color/toText themeMetadata.statusBar.info.foreground}"
    good_bg = "${Theme/Color/toText themeMetadata.statusBar.good.background}"
    good_fg = "${Theme/Color/toText themeMetadata.statusBar.good.foreground}"
    warning_bg = "${Theme/Color/toText
                      themeMetadata.statusBar.warning.background}"
    warning_fg = "${Theme/Color/toText
                      themeMetadata.statusBar.warning.foreground}"
    critical_bg = "${Theme/Color/toText
                       themeMetadata.statusBar.critical.background}"
    critical_fg = "${Theme/Color/toText
                       themeMetadata.statusBar.critical.foreground}"
    separator = "\ue0b2"
    # seperator_bg = "auto"
    # separator_fg = "auto"
    alternating_tint_bg = "${Theme/Color/toText
                               themeMetadata.statusBar.alternatingTint.background}"
    alternating_tint_fg = "${Theme/Color/toText
                               themeMetadata.statusBar.alternatingTint.foreground}"

    [[block]]
    block = "disk_space"
    alias = "/"
    format = "{icon} {used}{percentage}"
    info_type = "used"
    interval = 20
    path = "/"
    unit = "GB"
    warning = 80
    alert = 90

    [[block]]
    block = "net"
    format = "{ip}"
    format_alt = "{ipv6}"
    interval = 5

    [[block]]
    block = "net"
    device = "tun0"
    format = ""
    format_alt = ""
    interval = 5

    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  [[block]]
                  block = "battery"
                  format = "{percentage}% {time}"
                  interval = 10

                  [[block]]
                  block = "custom"
                  command = ''' printf " " && ${Directory/toText
                                                   Directory.Bin}/df_power_now '''
                  interval = 1
                  ''
          else  None Text
        )}

    [[block]]
    block = "cpu"
    format = "{utilization}"
    interval = 1

    [[block]]
    block = "temperature"
    chip = "*-isa-*"
    collapsed = false
    format = "{max}"
    interval = 10

    [[block]]
    block = "load"
    format = "{1m}"
    interval = 1

    [[block]]
    block = "memory"
    clickable = true
    display_type = "memory"
    format_mem = "{mem_used} {mem_used_percents:1}"
    format_swap = "{swap_used} {swap_used_percents:1}"
    interval = 5

    ${External/Prelude.Text.default
        ( if    Configuration/equal env.configuration Configuration.Laptop
          then  Some
                  ''
                  [[block]]
                  block = "backlight"
                  # invert_icons = true
                  step_width = 5
                  ''
          else  None Text
        )}

    [[block]]
    block = "sound"
    step_width = 5
    show_volume_when_muted = true

    [[block]]
    block = "sound"
    device_kind = "source"
    step_width = 5
    show_volume_when_muted = true

    [[block]]
    block = "uptime"
    interval = 60

    ${External/Prelude.Text.default
        ( if    PackageManager/equal env.package_manager PackageManager.Pacman
          then  Some
                  ''
                  [[block]]
                  block = "pacman"
                  aur_command = "yay -Qua"
                  critical_updates_regex = "(linux|linux-lts|linux-zen|linux-hardened)"
                  format = "{both}"
                  format_singular = "{both}"
                  format_up_to_date = "{both}"
                  interval = 600
                  ''
          else  None Text
        )}

    [[block]]
    block = "time"
    format = "%Y-%m-%d %T"
    interval = 1
    timezone = "US/Eastern"
    ''
