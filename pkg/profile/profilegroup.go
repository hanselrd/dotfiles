package profile

import (
	"fmt"
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/shell"
)

type ProfileGroup interface {
	fmt.Stringer
	SystemProfile() SystemProfile
	UserProfile() UserProfile
}

type profileGroup struct {
	Name   string        `json:"name"`
	System SystemProfile `json:"system"`
	User   UserProfile   `json:"user"`
}

func NewProfileGroup(system SystemProfile, user UserProfile) ProfileGroup {
	return profileGroup{
		Name:   fmt.Sprintf("%s-%s", system, user),
		System: system,
		User:   user,
	}
}

func DefaultProfileGroup() ProfileGroup {
	stdout := lo.T2(lo.Must2(shell.Shell("uname -a"))).A
	if strings.Contains(strings.ToLower(stdout), "microsoft") {
		return WslBase
	}
	if strings.Contains(stdout, "Darwin") {
		return DarwinBase
	}
	return LinuxBase
}

func (p profileGroup) String() string {
	return p.Name
}

func (p profileGroup) SystemProfile() SystemProfile {
	return p.System
}

func (p profileGroup) UserProfile() UserProfile {
	return p.User
}
