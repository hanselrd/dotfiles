package structs

import (
	"encoding/json"
	"fmt"

	"github.com/hanselrd/dotfiles/lib/enums"
)

type Profile struct {
	System enums.SystemProfile
	User   enums.UserProfile
}

func NewProfile(system enums.SystemProfile, user enums.UserProfile) *Profile {
	return &Profile{
		System: system,
		User:   user,
	}
}

func (p Profile) String() string {
	return fmt.Sprintf("%s-%s", p.System.String(), p.User.String())
}

func (p Profile) MarshalText() ([]byte, error) {
	return []byte(p.String()), nil
}

func (p Profile) MarshalJSON() ([]byte, error) {
	return json.Marshal(p.String())
}
