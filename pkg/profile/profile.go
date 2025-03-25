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

//go:generate go run ../../internal/codegen/privilegelevelfuncmap.go Profile

func NewProfile(pl privilegelevel.PrivilegeLevel, name string) (Profile, error) {
	return ProfilePrivilegeLevelFuncMap.String(pl, name)
}
