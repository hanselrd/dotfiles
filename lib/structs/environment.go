package structs

import (
	"github.com/hanselrd/dotfiles/lib/enums"
)

type Environment struct {
	User     User                           `json:"user"`
	Theme    Theme                          `json:"theme"`
	Roles    map[enums.Role]map[string]bool `json:"roles"`
	Profiles EnvironmentProfiles            `json:"profiles"`
}

type EnvironmentProfiles struct {
	NixOS       []Profile `json:"nixos"`
	Darwin      []Profile `json:"darwin"`
	HomeManager []Profile `json:"homeManager"`
}
