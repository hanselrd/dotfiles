package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/pkg/dockercompose"
)

var dockerComposeCmd = &cobra.Command{
	Use:   "docker-compose",
	Short: "Docker Compose command",
	Long:  "Docker Compose command",
	Run: func(cmd *cobra.Command, args []string) {
		data := lo.Must(json.MarshalIndent(dockercompose.DockerCompose, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(dockerComposeCmd)
}
