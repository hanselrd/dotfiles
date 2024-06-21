package homeage

import (
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"
)

var HomeageCmd = &cobra.Command{
	Use:   "homeage",
	Short: "Homeage command",
	Long:  "Homeage command",
	Run: func(cmd *cobra.Command, args []string) {
		log.Info().Msg("homeage called")
	},
}

func init() {}
