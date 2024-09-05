package secret

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var SecretCmd = &cobra.Command{
	Use:   "secret",
	Short: "Secret command (deprecated)",
	Long:  "Secret command (deprecated)",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("secret called")
	},
}

func init() {}
