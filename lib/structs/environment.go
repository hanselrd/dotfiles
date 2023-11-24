package structs

import (
	"github.com/hanselrd/dotfiles/lib/enums"
)

type Environment struct {
	User     User                `json:"user"`
	Theme    Theme               `json:"theme"`
	Roles    EnvironmentRoles    `json:"roles"`
	Profiles EnvironmentProfiles `json:"profiles"`
}

type EnvironmentRoles struct {
	System map[enums.SystemRole]map[string]bool `json:"system"`
	User   map[enums.UserRole]map[string]bool   `json:"user"`
}

type EnvironmentProfiles struct {
	NixOS       []Profile `json:"nixos"`
	Darwin      []Profile `json:"darwin"`
	HomeManager []Profile `json:"homeManager"`
}
