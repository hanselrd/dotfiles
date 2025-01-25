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
		shell.Shell("choco.exe upgrade all {{.VerbosityVerboseShort}} -y")
		// shell.Shell("choco.exe install starship {{.VerbosityVerboseShort}} --force -y")
		// for _, f := range []string{".config/starship.toml", ".ssh/config"} {
		// 	shell.Shell(
		// 		fmt.Sprintf(
		// 			"cp {{.VerbosityVerboseShortN}} %s/%[3]s \"%[2]s/%[3]s\"",
		// 			environment.Environment.User.HomeDirectory,
		// 			environment.Environment.Extra.WinUser.HomeDirectory,
		// 			f,
		// 		),
		// 	)
		// 	shell.Shell(fmt.Sprintf("chmod {{.VerbosityQuietLongVerboseShortN}} u+w \"%s/%s\"",
		// 		environment.Environment.Extra.WinUser.HomeDirectory,
		// 		f))
		// }
		// for _, f := range []string{".vscode/extensions"} {
		// 	shell.Shell(
		// 		fmt.Sprintf(
		// 			"rsync {{.VerbosityVerboseShortN}} -CcavzPL %s/%s \"%s/%s\"",
		// 			environment.Environment.User.HomeDirectory,
		// 			f,
		// 			environment.Environment.Extra.WinUser.UserProfile,
		// 			filepath.Dir(f),
		// 		),
		// 	)
		// }
		// for _, f := range []string{"Code/User/settings.json"} {
		// 	shell.Shell(
		// 		fmt.Sprintf(
		// 			"cp {{.VerbosityVerboseShortN}} %s/%[3]s \"%[2]s/%[3]s\"",
		// 			environment.Environment.User.ConfigDirectory,
		// 			environment.Environment.Extra.WinUser.AppData,
		// 			f,
		// 		),
		// 	)
		// 	shell.Shell(fmt.Sprintf("chmod {{.VerbosityQuietLongVerboseShortN}} u+w \"%s/%s\"",
		// 		environment.Environment.Extra.WinUser.AppData,
		// 		f))
		// }
	},
}

func init() {
	WindowsCmd.AddCommand(bootstrapCmd)
}
