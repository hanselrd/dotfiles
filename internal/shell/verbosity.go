package shell

import (
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/pkg/flags"
)

func verbosityQuietVerbose(quietFlag, verboseFlag string) string {
	return lo.Ternary(
		flags.Quiet,
		quietFlag,
		lo.Ternary(
			flags.Verbose > 0,
			verboseFlag,
			"",
		),
	)
}

func verbosityQuietVerboseN(quietFlag, verboseFlag string) string {
	return lo.Ternary(
		flags.Quiet,
		quietFlag,
		lo.Ternary(
			flags.Verbose > 0,
			strings.TrimSpace(
				lo.TernaryF(
					!strings.HasPrefix(verboseFlag, "--") && strings.HasPrefix(verboseFlag, "-") &&
						len(verboseFlag) == 2,
					func() string {
						return "-" + strings.Repeat(verboseFlag[1:], flags.Verbose)
					},
					func() string {
						return strings.Join(
							lo.RepeatBy(flags.Verbose, func(_ int) string {
								return verboseFlag
							}),
							" ")
					},
				)),
			"",
		),
	)
}
