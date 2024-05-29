package dotfiles

import (
	"fmt"
	"time"

	"github.com/itchyny/timefmt-go"

	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/profiles"
	"github.com/hanselrd/dotfiles/lib/structs"
	"github.com/hanselrd/dotfiles/lib/themes"
)

var (
	now           = time.Now()
	nowYmd        = timefmt.Format(now, "%Y%m%d")
	backupFileExt = fmt.Sprintf("bkp.%s", nowYmd)
	username      = "delacruz"
	homeDir       = fmt.Sprintf("/home/%s", username)
	configDir     = fmt.Sprintf("%s/.config", homeDir)
	cacheDir      = fmt.Sprintf("%s/.cache", homeDir)
)

var Environment = structs.Environment{
	User: structs.User{
		UserName:        username,
		Name:            "Hansel De La Cruz",
		Email:           "18725263+hanselrd@users.noreply.github.com",
		HomeDirectory:   homeDir,
		ConfigDirectory: configDir,
		CacheDirectory:  cacheDir,
	},
	Theme: themes.ThemeMapStruct[enums.ThemeChalk],
	Roles: structs.EnvironmentRoles{
		User: map[enums.UserRole]map[string]bool{
			enums.UserRoleHomeage: {
				"decrypt": false,
			},
			enums.UserRoleNix: {
				"sandbox": true,
			},
			enums.UserRoleShell: {
				"bashToZsh": false,
				"ldPreload": false,
				"rts":       false,
				"theme":     false,
			},
		},
	},
	Profiles: structs.EnvironmentProfiles{
		NixOS:       profiles.NixOSProfiles,
		Darwin:      profiles.DarwinProfiles,
		HomeManager: profiles.HomeManagerProfiles,
	},
	Extra: structs.EnvironmentExtra{
		BackupFileExtension: backupFileExt,
	},
}
