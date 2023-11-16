package cmd

import (
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var roleImportCmd = &cobra.Command{
	Use:   "roleImport",
	Short: "Role Import command",
	Long:  "Role Import command",
	Run: func(cmd *cobra.Command, args []string) {
		log.Info().Msg("roleImport called")
	},
}

func init() {
	rootCmd.AddCommand(roleImportCmd)
}
