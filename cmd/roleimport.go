package cmd

import (
	"os"
	"text/template"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/enums"
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
		// log.Fatal().Msg("roleImport called")
		// log.Panic().Msg("roleImport called")

		tmpl, err := template.ParseGlob("templates/*.gotmpl")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(os.Stdout, "roles.nix.gotmpl", enums.Roles())
		cobra.CheckErr(err)
	},
}

func init() {
	rootCmd.AddCommand(roleImportCmd)
}
