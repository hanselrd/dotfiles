package enums

import "strings"

type UserProfile uint

//go:generate go run github.com/dmarkham/enumer -type UserProfile -trimprefix UserProfile -linecomment -json -text

const (
	UserProfileBase UserProfile = iota
	UserProfileStandard
	UserProfileMinimal
	UserProfileFull
)

func (p UserProfile) Type() string {
	return "user"
}

func (p UserProfile) Profile() string {
	return strings.ToLower(p.String())
}
