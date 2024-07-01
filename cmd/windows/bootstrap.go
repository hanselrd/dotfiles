//go:build windows

package windows

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var bootstrapCmd = &cobra.Command{
	Use:   "bootstrap",
	Short: "Bootstrap command",
	Long:  "Bootstrap command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("bootstrap called")
	},
}

func init() {
	WindowsCmd.AddCommand(bootstrapCmd)
}
