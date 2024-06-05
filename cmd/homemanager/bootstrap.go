package homemanager

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/generic"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		shell.Shell(
			fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", _profile),
		)
		stdout := generic.First(
			generic.Must2(
				shell.Shell(
					fmt.Sprintf(
						"nix path-info .#homeConfigurations.%s.activationPackage",
						_profile,
					),
				),
			),
		)

		homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", stdout)
		stdout, _, err := shell.Shell(
			fmt.Sprintf(
				"%s switch --flake .#%s -b %s",
				homeManagerExe,
				_profile,
				environment.Environment.Extra.BackupFileExtension,
			),
		)
		if err != nil {
			stdout := generic.First(
				generic.Must2(
					shell.Shell(
						"sed -rn \"s/^.*Existing file '(.*)' .*$/\\1/p\"",
						shell.WithStdin(stdout),
					),
				),
			)
			files := strings.Split(stdout, "\n")
			for _, file := range files {
				err = os.Remove(file)
				cobra.CheckErr(err)
			}
			shell.Shell(
				fmt.Sprintf(
					"%s switch --flake .#%s -b %s",
					homeManagerExe,
					_profile,
					environment.Environment.Extra.BackupFileExtension,
				),
			)
		}
	},
}

func init() {
	HomeManagerCmd.AddCommand(bootstrapCmd)
}
