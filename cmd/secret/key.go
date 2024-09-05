package secret

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
				"age -d -i internal/assets/keys/0.age internal/assets/keys/1.age > %s/.keys/2.age",
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	SecretCmd.AddCommand(keyCmd)
}
