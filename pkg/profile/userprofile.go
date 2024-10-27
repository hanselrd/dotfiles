package profile

import "github.com/hanselrd/dotfiles/internal/privilegelevel"

type UserProfile uint

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile -linecomment -json -text -transform lower

const (
	UserProfileBase UserProfile = iota
	UserProfileStandard
	UserProfileMinimal
	UserProfileFull
)

func (p UserProfile) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelUser
}
