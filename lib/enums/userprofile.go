package enums

import "encoding/json"

type UserProfile uint

//go:generate stringer -type UserProfile -linecomment

const (
	UserProfileBase     UserProfile = iota // base
	UserProfileStandard                    // standard
	UserProfileMinimal                     // minimal
	UserProfileFull                        // full
)

func (u UserProfile) MarshalText() ([]byte, error) {
	return []byte(u.String()), nil
}

func (u UserProfile) MarshalJSON() ([]byte, error) {
	return json.Marshal(u.String())
}
