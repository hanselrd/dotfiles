package homeage

import (
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
)

var secretCmd = &cobra.Command{
	Use:   "secret",
	Short: "Secret command",
	Long:  "Secret command",
	Run: func(cmd *cobra.Command, args []string) {
		shell.Shell(
			"find user/roles/homeage/secrets -type f -not -name \"*.age\" -print -exec sh -c \"age -a -R user/roles/homeage/keys/1.age.pub {} > {}.age\" \\;",
		)
	},
}

func init() {
	HomeageCmd.AddCommand(secretCmd)
}
