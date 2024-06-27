package shellx

import (
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/pkg/flags"
)

func Verbosity(quietFlag, verboseFlag string) string {
	return lo.Ternary(
		flags.Quiet,
		quietFlag,
		lo.Ternary(
			flags.Verbose > 0,
			strings.TrimSpace(
				strings.Join(
					lo.RepeatBy(flags.Verbose, func(_ int) string {
						return verboseFlag
					}),
					" ")),
			"",
		),
	)
}

func VerbosityQuietOnly(quietFlag string) string {
	return Verbosity(quietFlag, "")
}

func VerbosityVerboseOnly(verboseFlag string) string {
	return Verbosity("", verboseFlag)
}

func VerbosityDefault() string {
	return Verbosity("--quiet", "-v")
}

func VerbosityQuietOnlyDefault() string {
	return VerbosityQuietOnly("--quiet")
}

func VerbosityVerboseOnlyDefault() string {
	return VerbosityVerboseOnly("-v")
}
