package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var environmentCmd = &cobra.Command{
	Use:   "environment",
	Short: "Environment command",
	Long:  "Environment command",
	Run: func(cmd *cobra.Command, args []string) {
		data := utils.Must(json.MarshalIndent(dotfiles.Environment, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(environmentCmd)
}
