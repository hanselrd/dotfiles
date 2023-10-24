package cmd

import (
	"os"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/homeage"
	"github.com/hanselrd/dotfiles/cmd/homemanager"
	"github.com/hanselrd/dotfiles/lib/flags"
)

var rootCmd = &cobra.Command{
	Use:   "dotfiles-cli",
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
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
