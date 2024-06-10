package homemanager

import (
	"fmt"
	"os"
	"regexp"

	"github.com/rs/zerolog/log"
	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

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
		stdout := lo.T2(
			lo.Must2(
				shell.Shell(
					fmt.Sprintf(
						"nix path-info .#homeConfigurations.%s.activationPackage",
						_profile,
					),
				),
			),
		).A

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
			re := regexp.MustCompile(`Existing file '(.*)' is in the way of '.*'\n`)
			matches := re.FindAllStringSubmatch(stdout, -1)
			lop.ForEach(matches, func(m []string, _ int) {
				log.Debug().Str("file", m[1]).Msg("removing")
				os.RemoveAll(m[1])
			})

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
