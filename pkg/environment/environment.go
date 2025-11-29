package environment

import (
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/pkg/profile"
	"github.com/hanselrd/dotfiles/pkg/theme"
)

type environment struct {
	User     environmentUser     `json:"user"`
	Theme    theme.Theme         `json:"theme"`
	Roles    environmentRoles    `json:"roles"`
	Profiles environmentProfiles `json:"profiles"`
	Extra    environmentExtra    `json:"extra"`
}

type environmentUser struct {
	UserName        string `json:"username"`
	Name            string `json:"name"`
	Email           string `json:"email"`
	HomeDirectory   string `json:"homeDirectory"`
	ConfigDirectory string `json:"configDirectory"`
	CacheDirectory  string `json:"cacheDirectory"`
	DataDirectory   string `json:"dataDirectory"`
	StateDirectory  string `json:"stateDirectory"`
}

type environmentProfiles struct {
	NixOS       []profile.ProfileGroup `json:"nixos"`
	Garuda      []profile.ProfileGroup `json:"garuda"`
	Wsl         []profile.ProfileGroup `json:"wsl"`
	Darwin      []profile.ProfileGroup `json:"darwin"`
	Generic     []profile.ProfileGroup `json:"generic"`
	HomeManager []profile.ProfileGroup `json:"homeManager"`
}

type environmentExtra struct {
	Encrypted           map[encryption.Encryption]bool `json:"encrypted"`
	WithSystemd         bool                           `json:"withSystemd"`
	BackupFileExtension string                         `json:"backupFileExtension"`
	TimeFormat          string                         `json:"timeFormat"`
	GoTimeFormat        string                         `json:"goTimeFormat"`
	WinUser             *environmentWinUser            `json:"winUser,omitempty"`
}

type environmentWinUser struct {
	environmentUser
	UserProfile  string `json:"userProfile"`
	AppData      string `json:"appData"`
	LocalAppData string `json:"localAppData"`
}
