package role

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type Role interface {
	fmt.Stringer
	NixString() string
	PrivilegeLevel() privilegelevel.PrivilegeLevel
	Enabled() bool
	Encryption() encryption.Encryption
}

func NewRole(pl privilegelevel.PrivilegeLevel, name string) (Role, error) {
	switch pl {
	case privilegelevel.PrivilegeLevelSystem:
		return SystemRoleString(name)
	case privilegelevel.PrivilegeLevelUser:
		return UserRoleString(name)
	}
	return nil, fmt.Errorf("could not create %s %s role", name, pl)
}
