package agenix

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var AgenixCmd = &cobra.Command{
	Use:   "agenix",
	Short: "Agenix command",
	Long:  "Agenix command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("agenix called")
	},
}

func init() {}
