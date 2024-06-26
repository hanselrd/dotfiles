package environment

import (
	"fmt"
	"os"
	"time"

	"github.com/itchyny/timefmt-go"

	"github.com/hanselrd/dotfiles/internal/hash"
	"github.com/hanselrd/dotfiles/internal/shell"
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
}

type environmentProfiles struct {
	Nixos       []profile.ProfileGroup `json:"nixos"`
	Darwin      []profile.ProfileGroup `json:"darwin"`
	Linux       []profile.ProfileGroup `json:"linux"`
	Wsl         []profile.ProfileGroup `json:"wsl"`
	HomeManager []profile.ProfileGroup `json:"homeManager"`
}

type environmentExtra struct {
	WithSystemd         bool             `json:"withSystemd"`
	BackupFileExtension string           `json:"backupFileExtension"`
	WinUser             *environmentUser `json:"winUser,omitempty"`
}

var (
	now           = time.Now()
	nowYmd        = timefmt.Format(now, "%Y%m%d")
	backupFileExt = fmt.Sprintf("bkp.%s-%s", nowYmd, hash.TodSeconds(now))
	userName      = "delacruz"
	name          = "Hansel De La Cruz"
	email         = "18725263+hanselrd@users.noreply.github.com"
	homeDir       = fmt.Sprintf("/home/%s", userName)
	configDir     = fmt.Sprintf("%s/.config", homeDir)
	cacheDir      = fmt.Sprintf("%s/.cache", homeDir)
)

var Environment = environment{
	User: environmentUser{
		UserName:        userName,
		Name:            name,
		Email:           email,
		HomeDirectory:   homeDir,
		ConfigDirectory: configDir,
		CacheDirectory:  cacheDir,
	},
	Theme: theme.Chalk,
	Roles: environmentRoles{
		System: &environmentRolesSystem{
			Networking: &environmentRolesSystemNetworking{
				HostName: "nohost0",
			},
		},
		User: &environmentRolesUser{
			Homeage: &environmentRolesUserHomeage{
				Decrypt: false,
			},
			Nix: &environmentRolesUserNix{
				Sandbox: true,
			},
			Shell: &environmentRolesUserShell{
				BashToZsh: false,
				LdPreload: false,
				Rts:       false,
				Theme:     false,
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
		WinUser: func() *environmentUser {
			if winUserName, _, err := shell.Shell("powershell.exe '$env:UserName'"); err == nil {
				winHomeDir := fmt.Sprintf("/mnt/c/Users/%s", winUserName)
				winConfigDir := fmt.Sprintf("%s/.config", winHomeDir)
				winCacheDir := fmt.Sprintf("%s/.cache", winHomeDir)
				return &environmentUser{
					UserName:        winUserName,
					Name:            name,
					Email:           email,
					HomeDirectory:   winHomeDir,
					ConfigDirectory: winConfigDir,
					CacheDirectory:  winCacheDir,
				}
			}
			return nil
		}(),
	},
}
