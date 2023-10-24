package homemanager

import (
	"fmt"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/flags"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		now := time.Now()
		nowYmd := timefmt.Format(now, "%Y%m%d")

		utils.Shell(
			fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", profile),
			flags.Dryrun, nil)
		_, stdout, _ := utils.Shell(
			fmt.Sprintf("nix path-info .#homeConfigurations.%s.activationPackage", profile),
			flags.Dryrun, nil)

		homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", stdout)
		utils.Shell(
			fmt.Sprintf("%s switch --flake .#%s -b bak.%s", homeManagerExe, profile, nowYmd),
			flags.Dryrun, nil)
	},
}

func init() {
	HomeManagerCmd.AddCommand(bootstrapCmd)
}
