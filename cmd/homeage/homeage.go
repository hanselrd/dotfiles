package homeage

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var HomeageCmd = &cobra.Command{
	Use:   "homeage",
	Short: "Homeage command",
	Long:  "Homeage command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("homeage called")
	},
}

func init() {}
