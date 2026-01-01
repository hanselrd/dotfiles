package log

import (
	"io"
	"log/slog"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/x/ansi"
	"github.com/fatih/color"
	"github.com/rs/zerolog"
	"github.com/samber/lo"
	slogzerolog "github.com/samber/slog-zerolog/v2"
)

var now = time.Now()

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
		FormatTimestamp: func(i any) string {
			if s, ok := i.(string); ok {
				t := lo.Must(time.ParseInLocation(zerolog.TimeFieldFormat, s, time.Local))
				w := 15
				return color.HiBlackString(
					"%s [%s]",
					t.In(time.Local).Format(time.Kitchen),
					lipgloss.PlaceHorizontal(
						w,
						lipgloss.Center,
						ansi.Truncate(time.Since(now).String(), w, "+"),
					),
				)
			}
			panic(i)
		},
		FormatLevel: func(i any) string {
			attrs := []color.Attribute{color.Bold}
			switch i.(string) {
			case zerolog.LevelTraceValue:
				attrs = append(attrs, color.FgHiBlack)
			case zerolog.LevelDebugValue:
				attrs = append(attrs, color.FgCyan)
			case zerolog.LevelInfoValue:
				attrs = append(attrs, color.FgGreen)
			case zerolog.LevelWarnValue:
				attrs = append(attrs, color.FgYellow)
			case zerolog.LevelErrorValue:
				attrs = append(attrs, color.FgRed)
			case zerolog.LevelFatalValue:
				attrs = append(attrs, color.FgHiRed, color.BlinkSlow)
			case zerolog.LevelPanicValue:
				attrs = append(attrs, color.BgHiRed, color.BlinkRapid)
			default:
				attrs = []color.Attribute{color.Reset}
			}
			return color.New(attrs...).Sprintf("%-5s", i) + "|"
		},
		// FormatCaller
		FormatMessage: func(i any) string {
			if i != nil {
				return color.HiWhiteString("%s", i)
			}
			return ""
		},
		FormatFieldName: func(i any) string {
			if s, ok := i.(string); ok {
				if strings.HasPrefix(s, "@") {
					return color.HiWhiteString("%s", s[:1]) +
						color.HiBlackString("%s= ", s[1:])
				}
			}
			return color.HiBlackString("%s= ", i)
		},
		FormatFieldValue: func(i any) string {
			return color.HiWhiteString("%s", i)
		},
		// FormatErrFieldName
		// FormatErrFieldValue
	})
	return slogzerolog.Option{
		Level:       opts.Level,
		Logger:      &logger,
		AddSource:   opts.AddSource,
		ReplaceAttr: opts.ReplaceAttr,
	}.NewZerologHandler()
}
