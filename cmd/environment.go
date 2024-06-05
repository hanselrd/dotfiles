package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/generic"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var environmentCmd = &cobra.Command{
	Use:   "environment",
	Short: "Environment command",
	Long:  "Environment command",
	Run: func(cmd *cobra.Command, args []string) {
		data := generic.Must(json.MarshalIndent(environment.Environment, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(environmentCmd)
}
