package enums

type SystemProfile uint

//go:generate stringer -type SystemProfile -linecomment
//go:generate go run codegen.go SystemProfile

const (
	SystemProfileNixos SystemProfile = iota // nixos
	SystemProfileMacos                      // macos
	SystemProfileLinux                      // linux
	SystemProfileWsl                        // wsl
)

func (p SystemProfile) Type() string {
	return "system"
}

func (p SystemProfile) Profile() string {
	return p.String()
}
