package nix

import (
	"fmt"
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/internal/shellx"
)

func build(installables ...string) {
	lo.Must2(shell.Shell(
		fmt.Sprintf(
			"nix build %s --no-link %s",
			shellx.VerbosityDefault(),
			strings.Join(installables, " "),
		),
	))
}

func findStorePaths(installables ...string) []string {
	return strings.Split(lo.T2(
		lo.Must2(shell.Shell(
			fmt.Sprintf(
				"nix path-info %s %s",
				shellx.VerbosityDefault(),
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
					"nix-store --query %s -R %s",
					shellx.VerbosityDefault(),
					strings.Join(paths, " "),
				),
			),
		),
	).A, "\n")
}
