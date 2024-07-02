package windows

import (
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		shell.Shell(
			"powershell.exe -Command \"& {Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))}\"",
		)
		// shell.Shell("choco.exe upgrade chocolatey")
		// shell.Shell("choco.exe upgrade all -y")
	},
}

func init() {
	WindowsCmd.AddCommand(bootstrapCmd)
}
