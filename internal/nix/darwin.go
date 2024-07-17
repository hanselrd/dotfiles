package nix

import (
	"fmt"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildDarwinConfiguration(pg profile.ProfileGroup) {
	build(
		fmt.Sprintf(".#darwinConfigurations.%s.config.system.build.toplevel", pg),
	)
}
