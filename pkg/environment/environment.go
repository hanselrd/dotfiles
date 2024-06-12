package environment

import (
	"fmt"
	"os"
	"time"

	"github.com/hanselrd/dotfiles/internal/hash"
	"github.com/hanselrd/dotfiles/pkg/profile"
	"github.com/hanselrd/dotfiles/pkg/role"
	"github.com/hanselrd/dotfiles/pkg/theme"

	"github.com/itchyny/timefmt-go"
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
}

type environmentRoles struct {
	System map[role.SystemRole]map[string]bool `json:"system"`
	User   map[role.UserRole]map[string]bool   `json:"user"`
}

type environmentProfiles struct {
	Nixos       []profile.ProfileGroup `json:"nixos"`
	Darwin      []profile.ProfileGroup `json:"darwin"`
	Linux       []profile.ProfileGroup `json:"linux"`
	Wsl         []profile.ProfileGroup `json:"wsl"`
	HomeManager []profile.ProfileGroup `json:"homeManager"`
}

type environmentExtra struct {
	WithSystemd         bool   `json:"withSystemd"`
	BackupFileExtension string `json:"backupFileExtension"`
}

var (
	now           = time.Now()
	nowYmd        = timefmt.Format(now, "%Y%m%d")
	backupFileExt = fmt.Sprintf("bkp.%s-%s", nowYmd, hash.TodSeconds(now))
	username      = "delacruz"
	homeDir       = fmt.Sprintf("/home/%s", username)
	configDir     = fmt.Sprintf("%s/.config", homeDir)
	cacheDir      = fmt.Sprintf("%s/.cache", homeDir)
)

var Environment = environment{
	User: environmentUser{
		UserName:        username,
		Name:            "Hansel De La Cruz",
		Email:           "18725263+hanselrd@users.noreply.github.com",
		HomeDirectory:   homeDir,
		ConfigDirectory: configDir,
		CacheDirectory:  cacheDir,
	},
	Theme: theme.Chalk,
	Roles: environmentRoles{
		User: map[role.UserRole]map[string]bool{
			role.UserRoleHomeage: {
				"decrypt": false,
			},
			role.UserRoleNix: {
				"sandbox": true,
			},
			role.UserRoleShell: {
				"bashToZsh": false,
				"ldPreload": false,
				"rts":       false,
				"theme":     false,
			},
		},
	},
	Profiles: environmentProfiles{
		Nixos:       profile.NixosProfiles,
		Darwin:      profile.DarwinProfiles,
		Linux:       profile.LinuxProfiles,
		Wsl:         profile.WslProfiles,
		HomeManager: profile.HomeManagerProfiles,
	},
	Extra: environmentExtra{
		WithSystemd: func() bool {
			if _, err := os.Stat("/run/systemd/system"); !os.IsNotExist(err) {
				return true
			}
			return false
		}(),
		BackupFileExtension: backupFileExt,
	},
}
