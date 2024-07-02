package windows

import (
	"fmt"

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
			"powershell.exe -Command \"& {Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))}\"",
		)
		shell.Shell("choco.exe upgrade all {{.VerbosityVerboseShort}} -y")
		shell.Shell("choco.exe install starship {{.VerbosityVerboseShort}} --force -y")
		shell.Shell(
			fmt.Sprintf(
				"cp {{.VerbosityVerboseShortN}} %s/.config/starship.toml \"%s/.config/starship.toml\"",
				environment.Environment.User.HomeDirectory,
				environment.Environment.Extra.WinUser.HomeDirectory,
			),
		)
	},
}

func init() {
	WindowsCmd.AddCommand(bootstrapCmd)
}
