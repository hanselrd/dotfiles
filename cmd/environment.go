//go:build !windows

package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/pkg/environment"
)

var environmentCmd = &cobra.Command{
	Use:   "environment",
	Short: "Environment command",
	Long:  "Environment command",
	Run: func(cmd *cobra.Command, args []string) {
		data := lo.Must(json.MarshalIndent(environment.Environment, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(environmentCmd)
}
