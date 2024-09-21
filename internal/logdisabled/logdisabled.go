package logdisabled

import (
	"io"
	"log/slog"
)

func init() {
	logger := slog.New(slog.NewTextHandler(io.Discard, nil))
	slog.SetDefault(logger)
}
