package homemanager

import (
	"fmt"
	"log/slog"
	"os"
	"slices"
	"strings"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/log"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var (
	_profile     string
	profileGroup profile.ProfileGroup
)

var HomeManagerCmd = &cobra.Command{
	Use:   "home-manager",
	Short: "Home Manager command",
	Long:  "Home Manager command",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) (err error) {
		idx := slices.IndexFunc(profile.HomeManagerProfiles, func(pg profile.ProfileGroup) bool {
			return pg.String() == _profile
		})
		if idx == -1 {
			err = fmt.Errorf("%s is not valid", _profile)
			slog.Error("", "error", err)
			return
		}
		profileGroup = profile.HomeManagerProfiles[idx]
		return
	},
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("home-manager called")
	},
}

func init() {
	slog.SetDefault(slog.New(log.NewHandler(os.Stderr,
		&slog.HandlerOptions{
			Level: log.LevelDisabled,
		},
	)))

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
