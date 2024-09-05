package environment

import (
	"fmt"
	"os"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/hash"
	"github.com/hanselrd/dotfiles/internal/log"
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
	Garuda      []profile.ProfileGroup `json:"garuda"`
	Wsl         []profile.ProfileGroup `json:"wsl"`
	Darwin      []profile.ProfileGroup `json:"darwin"`
	Linux       []profile.ProfileGroup `json:"linux"`
	HomeManager []profile.ProfileGroup `json:"homeManager"`
}

type environmentExtra struct {
	Encrypted           bool                `json:"encrypted"`
	WithSystemd         bool                `json:"withSystemd"`
	BackupFileExtension string              `json:"backupFileExtension"`
	TimeFormat          string              `json:"timeFormat"`
	GoTimeFormat        string              `json:"goTimeFormat"`
	WinUser             *environmentWinUser `json:"winUser,omitempty"`
}

type environmentWinUser struct {
	environmentUser
	UserProfile string `json:"userProfile"`
	AppData     string `json:"appData"`
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

var _ = func() int {
	log.SetupLogger(log.LevelDisabled)
	return 0
}()

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
			Nix: &environmentRolesUserNix{
				Sandbox: true,
			},
			Shell: &environmentRolesUserShell{
				BashToZsh: false,
				LdPreload: false,
				Theme:     false,
			},
		},
	},
	Profiles: environmentProfiles{
		Nixos:       profile.NixosProfiles,
		Garuda:      profile.GarudaProfiles,
		Wsl:         profile.WslProfiles,
		Darwin:      profile.DarwinProfiles,
		Linux:       profile.LinuxProfiles,
		HomeManager: profile.HomeManagerProfiles,
	},
	Extra: environmentExtra{
		Encrypted: lo.T2(shell.Shell("grep -vq \"false\" secrets/.encrypted")).A.ExitCode == 0,
		WithSystemd: func() bool {
			if _, err := os.Stat("/run/systemd/system"); !os.IsNotExist(err) {
				return true
			}
			return false
		}(),
		BackupFileExtension: backupFileExt,
		TimeFormat:          "%a %y/%-m/%-d T%H:%M",
		GoTimeFormat:        "Mon 06/1/2 T15:04",
		WinUser: func() *environmentWinUser {
			if res, err := shell.Shell("powershell.exe '$env:UserName'"); err == nil {
				winUserName := res.Stdout
				winHomeDir := fmt.Sprintf("/mnt/c/Users/%s", winUserName)
				winConfigDir := fmt.Sprintf("%s/.config", winHomeDir)
				winCacheDir := fmt.Sprintf("%s/.cache", winHomeDir)
				appData := fmt.Sprintf("%s/AppData/Roaming", winHomeDir)
				return &environmentWinUser{
					environmentUser: environmentUser{
						UserName:        winUserName,
						Name:            name,
						Email:           email,
						HomeDirectory:   winHomeDir,
						ConfigDirectory: winConfigDir,
						CacheDirectory:  winCacheDir,
					},
					UserProfile: winHomeDir,
					AppData:     appData,
				}
			}
			return nil
		}(),
	},
}
