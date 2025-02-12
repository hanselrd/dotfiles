package profile

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type Profile interface {
	fmt.Stringer
	NixString() string
	PrivilegeLevel() privilegelevel.PrivilegeLevel
}

func NewProfile(pl privilegelevel.PrivilegeLevel, name string) (Profile, error) {
	switch pl {
	case privilegelevel.PrivilegeLevelSystem:
		return SystemProfileString(name)
	case privilegelevel.PrivilegeLevelUser:
		return UserProfileString(name)
	}
	return nil, fmt.Errorf("could not create %s %s profile", name, pl)
}
