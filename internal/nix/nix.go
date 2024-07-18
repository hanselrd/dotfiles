package nix

import (
	"fmt"
	"strings"

	"github.com/hanselrd/dotfiles/internal/shell"
)

func build(installables ...string) (shell.ShellResult, error) {
	return shell.Shell(
		fmt.Sprintf(
			"nix build {{.VerbosityQuietLongVerboseShortN}} --no-link %s --impure",
			strings.Join(installables, " "),
		),
	)
}

func findStorePaths(installables ...string) ([]string, error) {
	res, err := shell.Shell(
		fmt.Sprintf(
			"nix path-info {{.VerbosityQuietLongVerboseShortN}} %s",
			strings.Join(installables, " "),
		),
	)
	if err != nil {
		return nil, err
	}
	return strings.Split(res.Stdout, "\n"), nil
}

func findStoreDependencies(paths ...string) ([]string, error) {
	res, err := shell.Shell(
		fmt.Sprintf(
			"nix-store --query {{.VerbosityQuietLongVerboseShortN}} -R %s",
			strings.Join(paths, " "),
		),
	)
	if err != nil {
		return nil, err
	}
	return strings.Split(res.Stdout, "\n"), nil
}
