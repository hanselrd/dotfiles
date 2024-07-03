package log

import (
	"context"
	"log/slog"
	"os"
)

const (
	LevelTrace    = LevelDebug - 1
	LevelDebug    = slog.LevelDebug
	LevelInfo     = slog.LevelInfo
	LevelWarn     = slog.LevelWarn
	LevelError    = slog.LevelError
	LevelFatal    = LevelError + 1
	LevelPanic    = LevelFatal + 1
	LevelDisabled = slog.Level(9999)
)

func Log(level slog.Level, msg string, args ...any) {
	slog.Log(context.Background(), level, msg, args...)
}

func Trace(msg string, args ...any) {
	Log(LevelTrace, msg, args...)
}

func Debug(msg string, args ...any) {
	Log(LevelDebug, msg, args...)
}

func Info(msg string, args ...any) {
	Log(LevelInfo, msg, args...)
}

func Warn(msg string, args ...any) {
	Log(LevelWarn, msg, args...)
}

func Error(msg string, args ...any) {
	Log(LevelError, msg, args...)
}

func Fatal(msg string, args ...any) {
	Log(LevelFatal, msg, args...)
	os.Exit(1)
}

func Panic(msg string, args ...any) {
	Log(LevelPanic, msg, args...)
	panic(msg)
}
