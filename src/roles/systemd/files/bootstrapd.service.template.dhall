let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    [Unit]
    Description=Bootstrap Daemon

    [Service]
    Environment=DISPLAY=:0
    Environment=XAUTHORITY=${env.user_home_dir}/.Xauthority
    ExecStartPre=${Directory/toText Directory.Systemd2}/bootstrapd_pre.sh
    ExecStart=${Directory/toText Directory.Systemd2}/bootstrapd.sh
    Restart=always

    [Install]
    WantedBy=default.target
    ''
