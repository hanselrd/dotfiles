package main

import (
	"fmt"
	"log/slog"

	"github.com/hanselrd/dotfiles/internal/build"
	"github.com/hanselrd/dotfiles/internal/log"
)

func main() {
	log.SetupLogger(log.LevelTrace)

	slog.Info(fmt.Sprintf("CANARY(%s)", build.Version))
}
