package profile

import "fmt"

type ProfileGroup interface {
	fmt.Stringer
}

type profileGroup struct {
	Name   string        `json:"name"`
	System SystemProfile `json:"system"`
	User   UserProfile   `json:"user"`
}

func NewProfileGroup(system SystemProfile, user UserProfile) *profileGroup {
	return &profileGroup{
		Name:   fmt.Sprintf("%s-%s", system, user),
		System: system,
		User:   user,
	}
}

func (p profileGroup) String() string {
	return p.Name
}
