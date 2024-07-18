package homemanager

import (
	"log/slog"
	"os"
	"regexp"

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

		res, err := nix.InstallHomeManagerConfiguration(profileGroup)
		if err != nil {
			re := regexp.MustCompile(`Existing file '(.*)' is in the way of '.*'\n`)
			matches := re.FindAllStringSubmatch(res.Stdout, -1)
			lop.ForEach(matches, func(m []string, _ int) {
				slog.Debug("removing", "file", m[1])
				os.RemoveAll(m[1])
			})

			nix.InstallHomeManagerConfiguration(profileGroup)
		}
	},
}

func init() {
	HomeManagerCmd.AddCommand(bootstrapCmd)
}
