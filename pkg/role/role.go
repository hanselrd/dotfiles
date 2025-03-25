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

//go:generate go run ../../internal/codegen/privilegelevelfuncmap.go Role

func NewRole(pl privilegelevel.PrivilegeLevel, name string) (Role, error) {
	return RolePrivilegeLevelFuncMap.String(pl, name)
}
