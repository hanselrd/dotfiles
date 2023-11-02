package homeage

import (
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/utils"
)

var secretCmd = &cobra.Command{
	Use:   "secret",
	Short: "Secret command",
	Long:  "Secret command",
	Run: func(cmd *cobra.Command, args []string) {
		utils.Shell("find core/user/role/homeage/secrets -type f -not -name \"*.age\" -print -exec sh -c \"age -a -R core/user/role/homeage/keys/1.age.pub {} > {}.age\" \\;")
	},
}

func init() {
	HomeageCmd.AddCommand(secretCmd)
}
