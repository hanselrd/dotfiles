package homemanager

import (
	"fmt"
	"slices"
	"strings"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var _profile string

var HomeManagerCmd = &cobra.Command{
	Use:   "homeManager",
	Short: "Home Manager command",
	Long:  "Home Manager command",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) (err error) {
		if !slices.ContainsFunc(profile.HomeManagerProfiles, func(pg profile.ProfileGroup) bool {
			return pg.String() == _profile
		}) {
			err = fmt.Errorf("%s is not valid", _profile)
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

	defaultProfile := func() profile.ProfileGroup {
		stdout := lo.T2(lo.Must2(shell.Shell("uname -a"))).A
		if strings.Contains(strings.ToLower(stdout), "microsoft") {
			return profile.WslBase
		}
		if strings.Contains(stdout, "Darwin") {
			return profile.DarwinBase
		}
		return profile.LinuxBase
	}()
	HomeManagerCmd.PersistentFlags().
		StringVar(&_profile, "profile", defaultProfile.String(), "home manager profile")
}
