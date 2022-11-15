let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

in  ''
    ${External/Prelude.Text.default
        ( if    Role/enabled Role.Systemd && Role/enabled Role.Xinit
          then  Some
                  ''
                  if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
                      exec startx
                  fi
                  ''
          else  None Text
        )}
    ''
