package homemanager

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/profiles"
)

var profile string

var HomeManagerCmd = &cobra.Command{
	Use:   "homeManager",
	Short: "Home Manager command",
	Long:  "Home Manager command",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("homeManager called")
	},
}

func init() {
	HomeManagerCmd.PersistentFlags().StringVar(&profile, "profile", profiles.LinuxBase.String(), "home manager profile")
}
