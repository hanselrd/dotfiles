package codegen

import (
	"log/slog"

	"github.com/spf13/cobra"
)

var CodegenCmd = &cobra.Command{
	Use:   "codegen",
	Short: "Codegen command",
	Long:  "Codegen command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("codegen called")
	},
}

func init() {}
