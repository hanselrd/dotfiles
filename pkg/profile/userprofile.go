package profile

import (
	"strings"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type UserProfile uint

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile
//go:generate go run ../../internal/codegen/nixstringee.go UserProfile

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
