package nix

import (
	"fmt"
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/shell"
)

func build(installables ...string) {
	lo.Must2(shell.Shell(
		fmt.Sprintf(
			"nix build {{.VerbosityQuietLongVerboseShortN}} --no-link %s",
			strings.Join(installables, " "),
		),
	))
}

func findStorePaths(installables ...string) []string {
	return strings.Split(lo.T2(
		lo.Must2(shell.Shell(
			fmt.Sprintf(
				"nix path-info {{.VerbosityQuietLongVerboseShortN}} %s",
				strings.Join(installables, " "),
			),
		)),
	).A, "\n")
}

func findStoreDependencies(paths ...string) []string {
	return strings.Split(lo.T2(
		lo.Must2(
			shell.Shell(
				fmt.Sprintf(
					"nix-store --query {{.VerbosityQuietLongVerboseShortN}} -R %s",
					strings.Join(paths, " "),
				),
			),
		),
	).A, "\n")
}
