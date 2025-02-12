package profile

import (
	"strings"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type SystemProfile uint

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile -linecomment -json -text

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
