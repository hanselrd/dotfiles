package cmd

import (
	"os"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/homeage"
	"github.com/hanselrd/dotfiles/cmd/homemanager"
	"github.com/hanselrd/dotfiles/lib/flags"
)

var rootCmd = &cobra.Command{
	Use:   "dotfiles-cli",
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		zerolog.SetGlobalLevel(zerolog.TraceLevel)
		log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
		return nil
	},
}

func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}

func init() {
	rootCmd.PersistentFlags().BoolVar(&flags.Dryrun, "dryrun", false, "run without affecting the system")

	rootCmd.AddCommand(homeage.HomeageCmd)
	rootCmd.AddCommand(homemanager.HomeManagerCmd)
}
