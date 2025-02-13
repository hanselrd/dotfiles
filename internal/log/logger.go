package log

import (
	"log/slog"
	"os"
	"sync"

	"github.com/fatih/color"
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

		logger := slog.New(NewHandler(os.Stderr,
			&slog.HandlerOptions{
				AddSource: false,
				Level:     &level,
			}))
		slog.SetDefault(logger)
	})
}

func init() {
	SetupLogger(LevelDisabled)
}
