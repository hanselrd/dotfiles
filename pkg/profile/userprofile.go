package profile

import (
	"strings"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile
//go:generate go run ../../internal/codegen/marshaler.go -type UserProfile -stringer NixString

type UserProfile uint

const (
	UserProfileBase UserProfile = iota
	UserProfileMinimal
	UserProfileStandard
	UserProfileFull
)

func (p UserProfile) NixString() string {
	return strings.ToLower(p.String())
}

func (p UserProfile) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelUser
}
