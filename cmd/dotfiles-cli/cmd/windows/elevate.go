package windows

import (
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
)

var elevateCmd = &cobra.Command{
	Use:   "elevate",
	Short: "Elevate command",
	Long:  "Elevate command",
	Run: func(cmd *cobra.Command, args []string) {
		shell.Shell("powershell.exe Start-Process -Verb runAs -FilePath wsl")
	},
}

func init() {
	WindowsCmd.AddCommand(elevateCmd)
}
