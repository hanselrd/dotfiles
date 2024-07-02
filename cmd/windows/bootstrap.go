package windows

import (
	"fmt"

	"github.com/samber/lo"
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
		winUserName := lo.T2(lo.Must2(shell.Shell("powershell.exe '$env:UserName'"))).A
		shell.Shell(
			fmt.Sprintf(
				"cp {{.VerbosityVerboseShortN}} %s/.config/starship.toml \"/mnt/c/Users/%s/.config/starship.toml\"",
				environment.Environment.User.HomeDirectory,
				winUserName,
			),
		)
	},
}

func init() {
	WindowsCmd.AddCommand(bootstrapCmd)
}
