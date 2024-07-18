package cmd

import (
	"log/slog"
	"os"
	"runtime"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/codegen"
	"github.com/hanselrd/dotfiles/cmd/homeage"
	"github.com/hanselrd/dotfiles/cmd/homemanager"
	"github.com/hanselrd/dotfiles/cmd/windows"
	"github.com/hanselrd/dotfiles/internal/log"
	"github.com/hanselrd/dotfiles/pkg/flags"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var rootCmd = &cobra.Command{
	Use:   "dotfiles-cli",
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		level := log.LevelDisabled
		switch flags.Quiet {
		case 0:
			switch flags.Verbose {
			case 0:
				level = log.LevelInfo
			case 1:
				level = log.LevelDebug
			case 2:
				level = log.LevelTrace
			default:
				level = log.LevelTrace - slog.Level(flags.Verbose) + 2
			}
		case 1:
			level = log.LevelWarn
		case 2:
			level = log.LevelError
		case 3:
			level = log.LevelFatal
		case 4:
			level = log.LevelPanic
		default:
			level = log.LevelPanic + slog.Level(flags.Quiet) - 4
		}
		log.SetupLogger(level)
		log.Log(
			level,
			"",
			"os",
			runtime.GOOS,
			"arch",
			runtime.GOARCH,
			"version",
			runtime.Version(),
			"cpu",
			runtime.NumCPU(),
		)
		slog.Info("flags",
			"dryrun", flags.Dryrun,
			"verbose", flags.Verbose,
			"quiet", flags.Quiet)
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
		CountVarP(&flags.Quiet, "quiet", "q", "quiet; do not generate unnecessary output")
	rootCmd.MarkFlagsMutuallyExclusive("verbose", "quiet")

	rootCmd.AddCommand(codegen.CodegenCmd)
	rootCmd.AddCommand(homeage.HomeageCmd)
	rootCmd.AddCommand(homemanager.HomeManagerCmd)
	if profile.DefaultProfileGroup().SystemProfile() == profile.SystemProfileWsl {
		rootCmd.AddCommand(windows.WindowsCmd)
	}
}
