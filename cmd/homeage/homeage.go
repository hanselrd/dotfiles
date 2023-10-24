package homeage

import (
	"fmt"

	"github.com/spf13/cobra"
)

var HomeageCmd = &cobra.Command{
	Use:   "homeage",
	Short: "Homeage command",
	Long:  "Homeage command",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("homeage called")
	},
}

func init() {
}
