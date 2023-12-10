package homemanager

import (
	"fmt"
	"os"
	"strings"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	sf "github.com/sa-/slicefunk"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/profiles"
	"github.com/hanselrd/dotfiles/lib/structs"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var profile string

var HomeManagerCmd = &cobra.Command{
	Use:   "homeManager",
	Short: "Home Manager command",
	Long:  "Home Manager command",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) (err error) {
		if len(sf.Filter(profiles.HomeManagerProfiles, func(p structs.Profile) bool {
			return p.String() == profile
		})) != 1 {
			err = fmt.Errorf("%s is not valid", profile)
			log.Error().Err(err).Send()
		}
		return
	},
	Run: func(cmd *cobra.Command, args []string) {
		log.Info().Msg("homeManager called")
	},
}

func init() {
	zerolog.SetGlobalLevel(zerolog.Disabled)

	defaultProfile := func() structs.Profile {
		stdout := utils.First(utils.Must2(utils.Shell("uname -a")))
		if strings.Contains(strings.ToLower(stdout), "microsoft") {
			return profiles.WSLBase
		}
		if strings.Contains(stdout, "Linux") {
			if _, err := os.Stat("/run/systemd/system"); !os.IsNotExist(err) {
				return profiles.LinuxSystemdBase
			}
		}
		return profiles.LinuxBase
	}()
	HomeManagerCmd.PersistentFlags().StringVar(&profile, "profile", defaultProfile.String(), "home manager profile")
}
