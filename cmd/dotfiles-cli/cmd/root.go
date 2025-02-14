package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"runtime"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/cmd/dotfiles-cli/cmd/codegen"
	"github.com/hanselrd/dotfiles/cmd/dotfiles-cli/cmd/homemanager"
	"github.com/hanselrd/dotfiles/cmd/dotfiles-cli/cmd/windows"
	"github.com/hanselrd/dotfiles/internal/build"
	"github.com/hanselrd/dotfiles/internal/config"
	"github.com/hanselrd/dotfiles/internal/log"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var rootCmd = &cobra.Command{
	Use: "dotfiles-cli",
	Version: fmt.Sprintf(
		`%s
pureEvalMode= %t
rootDir= %s`,
		build.Version,
		lo.IsNotEmpty(build.PureEvalMode),
		build.RootDir,
	),
	Short: "Dotfiles CLI",
	Long:  "Dotfiles CLI",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		level := log.LevelDisabled
		switch config.Quiet {
		case 0:
			switch config.Verbose {
			case 0:
				level = log.LevelInfo
			case 1:
				level = log.LevelDebug
			case 2:
				level = log.LevelTrace
			default:
				level = log.LevelTrace - slog.Level(config.Verbose) + 2
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
			level = log.LevelPanic + slog.Level(config.Quiet) - 4
		}
		log.SetupLogger(level)
		log.Log(
			level,
			"",
			"platform",
			fmt.Sprintf("%s/%s", runtime.GOOS, runtime.GOARCH),
			"runtime",
			runtime.Version(),
			"cpu",
			runtime.NumCPU(),
			"version",
			build.Version,
			"pureEvalMode",
			build.PureEvalMode,
			"rootDir",
			build.RootDir,
		)
		slog.Info("config",
			"dryrun", config.Dryrun,
			"verbose", config.Verbose,
			"quiet", config.Quiet)
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
		BoolVar(&config.Dryrun, "dryrun", false, "run without affecting the system")
	rootCmd.PersistentFlags().
		CountVarP(&config.Verbose, "verbose", "v", "output verbosity")
	rootCmd.PersistentFlags().
		CountVarP(&config.Quiet, "quiet", "q", "quiet; do not generate unnecessary output")
	rootCmd.MarkFlagsMutuallyExclusive("verbose", "quiet")

	rootCmd.AddCommand(codegen.CodegenCmd)
	rootCmd.AddCommand(homemanager.HomeManagerCmd)
	if profile.DefaultProfileGroup().SystemProfile() == profile.SystemProfileWsl {
		rootCmd.AddCommand(windows.WindowsCmd)
	}
}
