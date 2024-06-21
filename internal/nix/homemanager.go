package nix

import (
	"fmt"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildHomeManagerConfiguration(pg profile.ProfileGroup) {
	build(
		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", pg),
	)
}

func FindHomeManagerConfiguration(pg profile.ProfileGroup) string {
	return findStorePaths(
		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", pg))[0]
}

func InstallHomeManagerConfiguration(pg profile.ProfileGroup) (string, error) {
	hmc := FindHomeManagerConfiguration(pg)
	homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", hmc)
	stdout, _, err := shell.Shell(
		fmt.Sprintf(
			"%s switch --flake .#%s -b %s",
			homeManagerExe,
			pg,
			environment.Environment.Extra.BackupFileExtension,
		),
	)
	return stdout, err
}

func FindHomeManagerEjectPaths(pg profile.ProfileGroup) []string {
	return []string{
		FindHomeManagerConfiguration(pg),
		lo.T2(
			lo.Must2(
				shell.Shell(
					fmt.Sprintf(
						"readlink -f %s/.nix-profile",
						environment.Environment.User.HomeDirectory,
					),
				),
			),
		).A,
	}
}

func FindHomeManagerEjectDependencies(pg profile.ProfileGroup) []string {
	return findStoreDependencies(FindHomeManagerEjectPaths(pg)...)
}
