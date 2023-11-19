package cmd

import (
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/utils"
)

var roleImportCmd = &cobra.Command{
	Use:   "roleImport",
	Short: "Role Import command",
	Long:  "Role Import command",
	Run: func(cmd *cobra.Command, args []string) {
		log.Info().Msg("roleImport called")

		utils.Shell("ping -c 5 localhost")
		utils.Shell("ls go.mod does-not-exist")

		log.Trace().Msg("roleImport called")
		log.Debug().Msg("roleImport called")
		log.Info().Msg("roleImport called")
		log.Warn().Msg("roleImport called")
		log.Error().Msg("roleImport called")
		log.Fatal().Msg("roleImport called")
		log.Panic().Msg("roleImport called")
	},
}

func init() {
	rootCmd.AddCommand(roleImportCmd)
}
