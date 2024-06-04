package structs

import (
	"github.com/hanselrd/dotfiles/pkg/enums"
)

type Environment struct {
	User     User                `json:"user"`
	Theme    Theme               `json:"theme"`
	Roles    EnvironmentRoles    `json:"roles"`
	Profiles EnvironmentProfiles `json:"profiles"`
	Extra    EnvironmentExtra    `json:"extra"`
}

type EnvironmentRoles struct {
	System map[enums.SystemRole]map[string]bool `json:"system"`
	User   map[enums.UserRole]map[string]bool   `json:"user"`
}

type EnvironmentProfiles struct {
	Nixos       []Profile `json:"nixos"`
	Darwin      []Profile `json:"darwin"`
	Linux       []Profile `json:"linux"`
	Wsl         []Profile `json:"wsl"`
	HomeManager []Profile `json:"homeManager"`
}

type EnvironmentExtra struct {
	WithSystemd         bool   `json:"withSystemd"`
	BackupFileExtension string `json:"backupFileExtension"`
}
