package homemanager

import (
	"os"
	"regexp"

	"github.com/rs/zerolog/log"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/nix"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		nix.BuildHomeManagerConfiguration(profileGroup)

		stdout, err := nix.InstallHomeManagerConfiguration(profileGroup)
		if err != nil {
			re := regexp.MustCompile(`Existing file '(.*)' is in the way of '.*'\n`)
			matches := re.FindAllStringSubmatch(stdout, -1)
			lop.ForEach(matches, func(m []string, _ int) {
				log.Debug().Str("file", m[1]).Msg("removing")
				os.RemoveAll(m[1])
			})

			nix.InstallHomeManagerConfiguration(profileGroup)
		}
	},
}

func init() {
	HomeManagerCmd.AddCommand(bootstrapCmd)
}
