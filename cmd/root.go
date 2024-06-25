package cmd

import (
	"os"

	"github.com/fatih/color"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/homeage"
	"github.com/hanselrd/dotfiles/cmd/homemanager"
	"github.com/hanselrd/dotfiles/pkg/flags"
)

var rootCmd = &cobra.Command{
	Use:   "dotfiles-cli",
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		level := zerolog.Disabled
		if flags.Quiet {
			level = zerolog.ErrorLevel
		} else {
			switch flags.Verbose {
			case 0:
				level = zerolog.InfoLevel
			case 1:
				level = zerolog.DebugLevel
			default:
				level = zerolog.TraceLevel
			}
		}
		zerolog.SetGlobalLevel(level)
		log.Logger = log.Output(zerolog.ConsoleWriter{
			Out: os.Stderr,
			FormatLevel: func(i interface{}) string {
				var attrs []color.Attribute
				switch i.(string) {
				case zerolog.LevelTraceValue:
					attrs = []color.Attribute{color.FgHiBlack}
				case zerolog.LevelDebugValue:
					attrs = []color.Attribute{color.FgCyan}
				case zerolog.LevelInfoValue:
					attrs = []color.Attribute{color.FgGreen}
				case zerolog.LevelWarnValue:
					attrs = []color.Attribute{color.FgYellow}
				case zerolog.LevelErrorValue:
					attrs = []color.Attribute{color.FgRed}
				case zerolog.LevelFatalValue:
					attrs = []color.Attribute{color.FgHiRed, color.BlinkSlow}
				case zerolog.LevelPanicValue:
					attrs = []color.Attribute{color.BgHiRed, color.BlinkRapid}
				default:
					attrs = []color.Attribute{color.Reset}
				}
				return color.New(attrs...).Sprintf("%-5s", i) + "|"
			},
			FormatFieldName: func(i interface{}) string {
				return color.HiBlackString("%s= ", i)
			},
		})
		log.Info().Bool("dryrun", flags.Dryrun).Int("verbose", flags.Verbose).
			Bool("quiet", flags.Quiet).
			Msg("flags")
		return nil
	},
}

func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}

func init() {
	cobra.EnableTraverseRunHooks = true

	rootCmd.PersistentFlags().
		BoolVar(&flags.Dryrun, "dryrun", false, "run without affecting the system")
	rootCmd.PersistentFlags().
		CountVarP(&flags.Verbose, "verbose", "v", "output verbosity")
	rootCmd.PersistentFlags().
		BoolVarP(&flags.Quiet, "quiet", "q", false, "quiet; do not generate unnecessary output")
	rootCmd.MarkFlagsMutuallyExclusive("verbose", "quiet")

	rootCmd.AddCommand(homeage.HomeageCmd)
	rootCmd.AddCommand(homemanager.HomeManagerCmd)
}
