package structs

import (
	"fmt"

	"github.com/hanselrd/dotfiles/lib/enums"
)

type Profile struct {
	System enums.SystemProfile `json:"system"`
	User   enums.UserProfile   `json:"user"`
}

func NewProfile(system enums.SystemProfile, user enums.UserProfile) *Profile {
	return &Profile{
		System: system,
		User:   user,
	}
}

func (p Profile) String() string {
	return fmt.Sprintf("%s-%s", p.System, p.User)
}
