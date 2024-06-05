package homeage

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var keyCmd = &cobra.Command{
	Use:   "key",
	Short: "Key command",
	Long:  "Key command",
	Run: func(cmd *cobra.Command, args []string) {
		err := os.MkdirAll(
			fmt.Sprintf("%s/.keys", environment.Environment.User.HomeDirectory),
			0o700,
		)
		cobra.CheckErr(err)

		shell.Shell(
			fmt.Sprintf(
				"age -d -i user/roles/homeage/keys/0.age user/roles/homeage/keys/1.age > %s/.keys/2.age",
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	HomeageCmd.AddCommand(keyCmd)
}
