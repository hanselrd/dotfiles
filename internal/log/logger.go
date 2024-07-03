package log

import (
	"log/slog"
	"os"
	"sync"
)

var (
	level  slog.LevelVar
	logger *slog.Logger
	once   sync.Once
)

func SetupLogger(l slog.Level) {
	level.Set(l)
	once.Do(func() {
		logger := slog.New(NewHandler(os.Stderr,
			&slog.HandlerOptions{
				AddSource: true,
				Level:     &level,
			}))
		slog.SetDefault(logger)
	})
}
