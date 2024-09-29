package log

import (
	"io"
	"log/slog"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/rs/zerolog"
	"github.com/samber/lo"
	slogzerolog "github.com/samber/slog-zerolog/v2"

	"github.com/hanselrd/dotfiles/pkg/environment"
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
		FormatTimestamp: func(i interface{}) string {
			if s, ok := i.(string); ok {
				t := lo.Must(time.ParseInLocation(zerolog.TimeFieldFormat, s, time.Local))
				return color.HiBlackString(
					"%s ",
					t.In(time.Local).Format(environment.Environment.Extra.GoTimeFormat),
				) + color.HiWhiteString(
					"<%s>",
					time.Since(now),
				)
			}
			return color.HiBlackString("<%s>", time.Since(now))
		},
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
		// FormatCaller
		FormatMessage: func(i interface{}) string {
			if i != nil {
				return color.HiWhiteString("%s", i)
			}
			return ""
		},
		FormatFieldName: func(i interface{}) string {
			if s, ok := i.(string); ok {
				if strings.HasPrefix(s, "@") {
					return color.HiWhiteString("%s", s[:1]) +
						color.HiBlackString("%s= ", s[1:])
				}
			}
			return color.HiBlackString("%s= ", i)
		},
		FormatFieldValue: func(i interface{}) string {
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
