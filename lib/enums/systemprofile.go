package enums

type SystemProfile uint

//go:generate stringer -type SystemProfile -linecomment
//go:generate go run codegen.go SystemProfile

const (
	SystemProfileNixOS        SystemProfile = iota // nixos
	SystemProfileMacOS                             // macos
	SystemProfileLinuxSystemd                      // linux-systemd
	SystemProfileLinux                             // linux
	SystemProfileWSL                               // wsl
)

func (p SystemProfile) Type() string {
	return "system"
}

func (p SystemProfile) Profile() string {
	return p.String()
}
