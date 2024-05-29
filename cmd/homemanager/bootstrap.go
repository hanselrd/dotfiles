package homemanager

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		utils.Shell(fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", profile))
		stdout := utils.First(utils.Must2(utils.Shell(fmt.Sprintf("nix path-info .#homeConfigurations.%s.activationPackage", profile))))

		homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", stdout)
		stdout, _, err := utils.Shell(fmt.Sprintf("%s switch --flake .#%s -b %s", homeManagerExe, profile, dotfiles.Environment.Extra.BackupFileExtension))
		if err != nil {
			stdout := utils.First(utils.Must2(utils.Shell("sed -rn \"s/^.*Existing file '(.*)' .*$/\\1/p\"", utils.WithStdin(stdout))))
			files := strings.Split(stdout, "\n")
			for _, file := range files {
				err = os.Remove(file)
				cobra.CheckErr(err)
			}
			utils.Shell(fmt.Sprintf("%s switch --flake .#%s -b %s", homeManagerExe, profile, dotfiles.Environment.Extra.BackupFileExtension))
		}
	},
}

func init() {
	HomeManagerCmd.AddCommand(bootstrapCmd)
}
