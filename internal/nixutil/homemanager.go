package nixutil

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildHomeManagerConfiguration(pg profile.ProfileGroup) (shell.ShellResult, error) {
	return build(
		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", pg),
	)
}

func FindHomeManagerConfiguration(pg profile.ProfileGroup) (string, error) {
	paths, err := findStorePaths(
		fmt.Sprintf(".#homeConfigurations.%s.activationPackage", pg))
	if err != nil {
		return "", err
	}
	return paths[0], nil
}

func InstallHomeManagerConfiguration(pg profile.ProfileGroup) (shell.ShellResult, error) {
	hmc, err := FindHomeManagerConfiguration(pg)
	if err != nil {
		return shell.ShellResult{}, err
	}
	// homeManagerExe := fmt.Sprintf("%s/home-path/bin/home-manager", hmc)
	// return shell.Shell(
	// 	fmt.Sprintf(
	// 		"%s switch {{.VerbosityVerboseShort}} --flake .#%s -b %s",
	// 		homeManagerExe,
	// 		pg,
	// 		environment.Environment.Extra.BackupFileExtension,
	// 	),
	// )
	nhExe := fmt.Sprintf("%s/home-path/bin/nh", hmc)
	return shell.Shell(
		fmt.Sprintf(
			"%s home switch . {{.VerbosityQuietShortVerboseShortN}} -c %s -b %s",
			nhExe,
			pg,
			environment.Environment.Extra.BackupFileExtension,
		),
	)
}

func FindHomeManagerEjectPaths(pg profile.ProfileGroup) ([]string, error) {
	hmc, err := FindHomeManagerConfiguration(pg)
	if err != nil {
		return nil, err
	}
	res, err := shell.Shell(
		fmt.Sprintf(
			"readlink {{.VerbosityQuietLongVerboseShortN}} -f %s/.nix-profile",
			environment.Environment.User.HomeDirectory,
		),
	)
	if err != nil {
		return nil, err
	}
	return []string{hmc, res.Stdout}, nil
}

func FindHomeManagerEjectDependencies(pg profile.ProfileGroup) ([]string, error) {
	paths, err := FindHomeManagerEjectPaths(pg)
	if err != nil {
		return nil, err
	}
	return findStoreDependencies(paths...)
}
