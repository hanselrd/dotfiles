package homeage

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var keyCmd = &cobra.Command{
	Use:   "key",
	Short: "Key command",
	Long:  "Key command",
	Run: func(cmd *cobra.Command, args []string) {
		err := os.MkdirAll(fmt.Sprintf("%s/.keys", dotfiles.Environment.User.HomeDirectory), 0700)
		cobra.CheckErr(err)

		utils.Shell(
			fmt.Sprintf(
				"age -d -i user/roles/homeage/keys/0.age user/roles/homeage/keys/1.age > %s/.keys/2.age",
				dotfiles.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	HomeageCmd.AddCommand(keyCmd)
}
