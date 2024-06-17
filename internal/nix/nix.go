package nix

import (
	"fmt"
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

func build(installables ...string) {
	shell.Shell(
		fmt.Sprintf("nix build --no-link %s", strings.Join(installables, " ")),
	)
}

func findStorePath(installables ...string) string {
	return lo.T2(
		lo.Must2(
			shell.Shell(
				fmt.Sprintf("nix path-info %s", strings.Join(installables, " ")),
			),
		),
	).A
}

func BuildHomeManagerConfiguration(profile string) {
	build(

		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", profile),
	)
}

func FindHomeManagerConfiguration(profile string) string {
	return findStorePath(
		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", profile),
	)
}

func InstallHomeManagerConfiguration(profile string) (string, error) {
	hmcsp := FindHomeManagerConfiguration(profile)
	homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", hmcsp)
	stdout, _, err := shell.Shell(
		fmt.Sprintf(
			"%s switch --flake .#%s -b %s",
			homeManagerExe,
			profile,
			environment.Environment.Extra.BackupFileExtension,
		),
	)
	return stdout, err
}
