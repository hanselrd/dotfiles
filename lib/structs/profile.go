package structs

import (
	"fmt"
	"strings"

	"github.com/hanselrd/dotfiles/lib/enums"
)

type Profile struct {
	Name   string              `json:"name"`
	System enums.SystemProfile `json:"system"`
	User   enums.UserProfile   `json:"user"`
}

func NewProfile(system enums.SystemProfile, user enums.UserProfile) *Profile {
	return &Profile{
		Name:   strings.ToLower(fmt.Sprintf("%s-%s", system, user)),
		System: system,
		User:   user,
	}
}

func (p Profile) String() string {
	return p.Name
}
