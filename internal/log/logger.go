package log

import (
	"fmt"
	"log/slog"
	"os"
	"runtime"
	"sync"

	"github.com/fatih/color"

	"github.com/hanselrd/dotfiles/internal/build"
)

var (
	level  slog.LevelVar
	logger *slog.Logger
	once   sync.Once
)

func SetupLogger(l slog.Level) {
	level.Set(l)
	once.Do(func() {
		color.NoColor = false

		logger = slog.New(NewHandler(os.Stderr,
			&slog.HandlerOptions{
				AddSource: false,
				Level:     &level,
			}))
		slog.SetDefault(logger)
	})
	slog.Info(
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
}

func init() {
	SetupLogger(LevelDisabled)
}
