package windows

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var WindowsCmd = &cobra.Command{
	Use:   "windows",
	Short: "Windows command",
	Long:  "Windows command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("windows called")
	},
}
