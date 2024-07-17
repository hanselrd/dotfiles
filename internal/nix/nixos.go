package nix

import (
	"fmt"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildNixOSConfiguration(pg profile.ProfileGroup) {
	build(
		fmt.Sprintf(".#nixosConfigurations.%s.config.system.build.toplevel", pg),
	)
}
