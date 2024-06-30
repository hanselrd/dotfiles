package log

import (
	"context"
	"fmt"
	"io"
	"log/slog"
	"os"

	"github.com/fatih/color"
	"github.com/rs/zerolog"
	slogzerolog "github.com/samber/slog-zerolog/v2"
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
	slog.Log(context.Background(), level, msg,
		append([]any{
			fmt.Sprintf("@%s", slog.LevelKey),
			level,
		}, args...)...,
	)
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

func NewHandler(w io.Writer, opts *slog.HandlerOptions) slog.Handler {
	slogzerolog.LogLevels = map[slog.Level]zerolog.Level{
		LevelTrace:    zerolog.TraceLevel,
		LevelDebug:    zerolog.DebugLevel,
		LevelInfo:     zerolog.InfoLevel,
		LevelWarn:     zerolog.WarnLevel,
		LevelError:    zerolog.ErrorLevel,
		LevelFatal:    zerolog.FatalLevel,
		LevelPanic:    zerolog.PanicLevel,
		LevelDisabled: zerolog.Disabled,
	}
	logger := zerolog.New(zerolog.ConsoleWriter{
		Out: w,
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
	return slogzerolog.Option{
		Level:       opts.Level,
		Logger:      &logger,
		AddSource:   opts.AddSource,
		ReplaceAttr: opts.ReplaceAttr,
	}.NewZerologHandler()
}
