package shell

import (
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/config"
)

func verbosityQuietVerbose(quietFlag, verboseFlag string) string {
	return lo.Ternary(
		config.Quiet > 0,
		quietFlag,
		lo.Ternary(
			config.Verbose > 0,
			verboseFlag,
			"",
		),
	)
}

func verbosityQuietVerboseN(quietFlag, verboseFlag string) string {
	return lo.Ternary(
		config.Quiet > 0,
		quietFlag,
		lo.Ternary(
			config.Verbose > 0,
			strings.TrimSpace(
				lo.TernaryF(
					!strings.HasPrefix(verboseFlag, "--") && strings.HasPrefix(verboseFlag, "-") &&
						len(verboseFlag) == 2,
					func() string {
						return "-" + strings.Repeat(verboseFlag[1:], config.Verbose)
					},
					func() string {
						return strings.Join(
							lo.RepeatBy(config.Verbose, func(_ int) string {
								return verboseFlag
							}),
							" ")
					},
				)),
			"",
		),
	)
}
