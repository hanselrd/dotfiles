package nix

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildNixOSConfiguration(pg profile.ProfileGroup) (shell.ShellResult, error) {
	return build(
		fmt.Sprintf(".#nixosConfigurations.%s.config.system.build.toplevel", pg),
	)
}
