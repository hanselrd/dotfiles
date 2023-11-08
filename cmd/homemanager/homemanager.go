package homemanager

import (
	"fmt"

	"github.com/rs/zerolog/log"
	sf "github.com/sa-/slicefunk"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/profiles"
	"github.com/hanselrd/dotfiles/lib/structs"
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
		fmt.Println("homeManager called")
	},
}

func init() {
	HomeManagerCmd.PersistentFlags().StringVar(&profile, "profile", profiles.LinuxBase.String(), "home manager profile")
}
