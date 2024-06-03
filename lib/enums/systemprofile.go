package enums

import "strings"

type SystemProfile uint

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile -linecomment -json -text

const (
	SystemProfileNixOS SystemProfile = iota
	SystemProfileMacOS
	SystemProfileLinux
	SystemProfileWSL
)

func (p SystemProfile) Type() string {
	return "system"
}

func (p SystemProfile) Profile() string {
	return strings.ToLower(p.String())
}
