package profile

import (
	"strings"

	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type UserProfile uint

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile -linecomment -json -text

const (
	UserProfileBase UserProfile = iota
	UserProfileStandard
	UserProfileMinimal
	UserProfileFull
)

func (p UserProfile) NixString() string {
	return strings.ToLower(p.String())
}

func (p UserProfile) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelUser
}
