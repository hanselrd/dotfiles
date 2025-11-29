package environment

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/build"
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/hash"
	_ "github.com/hanselrd/dotfiles/internal/logdisabled"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
	"github.com/hanselrd/dotfiles/pkg/theme"
)

var (
	now           = time.Now()
	backupFileExt = fmt.Sprintf("bkp.%s%s", hash.Date(now), hash.TodSeconds(now))
	systemProfile = func() profile.SystemProfile {
		res := lo.Must(shell.Shell("uname -a"))
		if strings.Contains(strings.ToLower(res.Stdout), "microsoft") {
			return profile.SystemProfileWsl
		}
		if strings.Contains(res.Stdout, "Darwin") {
			return profile.SystemProfileDarwin
		}
		return profile.SystemProfileGeneric
	}()
	homeDirFn = func(userName string) string {
		if systemProfile == profile.SystemProfileDarwin {
			return fmt.Sprintf("/Users/%s", userName)
		}
		return fmt.Sprintf("/home/%s", userName)
	}
	configDirFn  = func(homeDir string) string { return fmt.Sprintf("%s/.config", homeDir) }
	cacheDirFn   = func(homeDir string) string { return fmt.Sprintf("%s/.cache", homeDir) }
	dataDirFn    = func(homeDir string) string { return fmt.Sprintf("%s/.local/share", homeDir) }
	stateDirFn   = func(homeDir string) string { return fmt.Sprintf("%s/.local/state", homeDir) }
	winHomeDirFn = func(winUserName string) string { return fmt.Sprintf("/mnt/c/Users/%s", winUserName) }
	userName     = "delacruz"
	name         = "Hansel De La Cruz"
	email        = "18725263+hanselrd@users.noreply.github.com"
	homeDir      = homeDirFn(userName)
	configDir    = configDirFn(homeDir)
	cacheDir     = cacheDirFn(homeDir)
	dataDir      = dataDirFn(homeDir)
	stateDir     = stateDirFn(homeDir)
	winUserName  = name
	winHomeDir   = winHomeDirFn(winUserName)
	winConfigDir = configDirFn(winHomeDir)
	winCacheDir  = cacheDirFn(winHomeDir)
	winDataDir   = dataDirFn(winHomeDir)
	winStateDir  = stateDirFn(winHomeDir)
)

var Environment = environment{
	User: environmentUser{
		UserName:        userName,
		Name:            name,
		Email:           email,
		HomeDirectory:   homeDir,
		ConfigDirectory: configDir,
		CacheDirectory:  cacheDir,
		DataDirectory:   dataDir,
		StateDirectory:  stateDir,
	},
	Theme: theme.Chalk,
	Roles: environmentRoles{
		System: &environmentRolesSystem{
			Grub: &environmentRolesSystemGrub{
				Device: "/dev/sda",
			},
			Language: &environmentRolesSystemLanguage{
				Locale:       "en_US.UTF-8",
				Charset:      "UTF-8",
				ExtraLocales: []string{"es_DO.UTF-8/UTF-8", "es_ES.UTF-8/UTF-8"},
			},
			Networking: &environmentRolesSystemNetworking{
				HostName: "nohost0",
			},
			SystemdBoot: &environmentRolesSystemSystemdBoot{
				Xbootldr: false,
			},
			Time: &environmentRolesSystemTime{
				TimeZone: "Etc/UTC",
			},
		},
		User: &environmentRolesUser{
			Nix: &environmentRolesUserNix{
				Sandbox: true,
			},
			Shell: &environmentRolesUserShell{
				BashToZsh: false,
				Theme:     false,
			},
			Time: &environmentRolesUserTime{
				TimeZone:          "America/New_York",
				TimeZoneDirectory: "/usr/share/zoneinfo",
			},
		},
	},
	Profiles: environmentProfiles{
		NixOS:       profile.NixOSProfiles,
		Garuda:      profile.GarudaProfiles,
		Wsl:         profile.WslProfiles,
		Darwin:      profile.DarwinProfiles,
		Generic:     profile.GenericProfiles,
		HomeManager: profile.HomeManagerProfiles,
	},
	Extra: environmentExtra{
		Encrypted: lo.Associate(
			lo.DropByIndex(encryption.EncryptionValues(), 0),
			func(e encryption.Encryption) (encryption.Encryption, bool) {
				file := ".encrypted"
				switch e {
				case encryption.EncryptionNone:
					// do nothing
				case encryption.EncryptionDefault:
					file = filepath.Join("secrets", file)
				default:
					file = filepath.Join(fmt.Sprintf("secrets/%s", e.NixString()), file)
				}
				file = filepath.Join(build.RootDir, file)
				if _, err := os.Stat(file); !os.IsNotExist(err) {
					return e, lo.T2(
						shell.Shell(fmt.Sprintf("grep -vq \"false\" %s", file)),
					).A.ExitCode == 0
				}
				return e, true
			},
		),
		WithSystemd: func() bool {
			if _, err := os.Stat("/run/systemd/system"); !os.IsNotExist(err) {
				return true
			}
			return false
		}(),
		BackupFileExtension: backupFileExt,
		TimeFormat:          "<%a>%-d-%b-%y <%Z>T%H:%M",
		GoTimeFormat:        "<Mon>2-Jan-06 <MST>T15:04",
		WinUser: &environmentWinUser{
			environmentUser: environmentUser{
				UserName:        winUserName,
				Name:            name,
				Email:           email,
				HomeDirectory:   winHomeDir,
				ConfigDirectory: winConfigDir,
				CacheDirectory:  winCacheDir,
				DataDirectory:   winDataDir,
				StateDirectory:  winStateDir,
			},
			UserProfile:  winHomeDir,
			AppData:      fmt.Sprintf("%s/AppData/Roaming", winHomeDir),
			LocalAppData: fmt.Sprintf("%s/AppData/Local", winHomeDir),
		},
	},
}
