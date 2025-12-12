package profile

import (
	"strings"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile
//go:generate go run ../../internal/codegen/marshaler.go -type SystemProfile -stringer NixString

type SystemProfile uint

const (
	SystemProfileNixOS SystemProfile = iota
	SystemProfileGaruda
	SystemProfileWsl
	SystemProfileDarwin
	SystemProfileGeneric
)

func (p SystemProfile) NixString() string {
	return strings.ToLower(p.String())
}

func (p SystemProfile) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelSystem
}
