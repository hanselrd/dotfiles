package enums

import "encoding/json"

type SystemProfile uint

//go:generate stringer -type SystemProfile -linecomment

const (
	SystemProfileNixOS        SystemProfile = iota // nixos
	SystemProfileMacOS                             // macos
	SystemProfileLinuxSystemd                      // linux-systemd
	SystemProfileLinux                             // linux
	SystemProfileWSL                               // wsl
)

func (s SystemProfile) MarshalText() ([]byte, error) {
	return []byte(s.String()), nil
}

func (s SystemProfile) MarshalJSON() ([]byte, error) {
	return json.Marshal(s.String())
}
