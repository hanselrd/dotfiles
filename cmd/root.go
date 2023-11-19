package cmd

import (
	"os"

	"github.com/fatih/color"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/homeage"
	"github.com/hanselrd/dotfiles/cmd/homemanager"
	"github.com/hanselrd/dotfiles/lib/flags"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var rootCmd = &cobra.Command{
	Use:   "dotfiles-cli",
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		var level zerolog.Level
		switch flags.LogLevel.String() {
		case zerolog.LevelTraceValue:
			fallthrough
		default:
			level = zerolog.TraceLevel
		case zerolog.LevelDebugValue:
			level = zerolog.DebugLevel
		case zerolog.LevelInfoValue:
			level = zerolog.InfoLevel
		case zerolog.LevelWarnValue:
			level = zerolog.WarnLevel
		case zerolog.LevelErrorValue:
			level = zerolog.ErrorLevel
		case zerolog.LevelFatalValue:
			level = zerolog.FatalLevel
		case zerolog.LevelPanicValue:
			level = zerolog.PanicLevel
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
		log.Info().Bool("dryrun", flags.Dryrun).Send()
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

	rootCmd.PersistentFlags().BoolVar(&flags.Dryrun, "dryrun", false, "run without affecting the system")
	flags.LogLevel = utils.NewChoice[zerolog.Level](
		[]zerolog.Level{
			zerolog.TraceLevel,
			zerolog.DebugLevel,
			zerolog.InfoLevel,
			zerolog.WarnLevel,
			zerolog.ErrorLevel,
			zerolog.FatalLevel,
			zerolog.PanicLevel,
		},
		zerolog.TraceLevel,
	)
	rootCmd.PersistentFlags().Var(flags.LogLevel, "log-level", "log level")

	rootCmd.AddCommand(homeage.HomeageCmd)
	rootCmd.AddCommand(homemanager.HomeManagerCmd)
}
