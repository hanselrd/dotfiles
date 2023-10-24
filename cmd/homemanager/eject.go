package homemanager

import (
	"fmt"

	"github.com/spf13/cobra"
)

var ejectCmd = &cobra.Command{
	Use:   "eject",
	Short: "Eject command",
	Long:  "Eject command",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("eject called")
	},
}

func init() {
	HomeManagerCmd.AddCommand(ejectCmd)
}
