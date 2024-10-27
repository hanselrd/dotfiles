package profile

import "github.com/hanselrd/dotfiles/internal/privilegelevel"

type SystemProfile uint

//go:generate go run github.com/dmarkham/enumer -type SystemProfile -trimprefix SystemProfile -linecomment -json -text -transform lower

const (
	SystemProfileNixos SystemProfile = iota
	SystemProfileGaruda
	SystemProfileWsl
	SystemProfileDarwin
	SystemProfileGeneric
)

func (p SystemProfile) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelSystem
}
