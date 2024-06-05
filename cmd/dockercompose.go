package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/generic"
	"github.com/hanselrd/dotfiles/pkg/dockercompose"
)

var dockerComposeCmd = &cobra.Command{
	Use:   "dockerCompose",
	Short: "Docker Compose command",
	Long:  "Docker Compose command",
	Run: func(cmd *cobra.Command, args []string) {
		data := generic.Must(json.MarshalIndent(dockercompose.DockerCompose, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(dockerComposeCmd)
}
