package nixutil

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

func BuildDarwinConfiguration(pg profile.ProfileGroup) (shell.ShellResult, error) {
	return build(
		fmt.Sprintf(".#darwinConfigurations.%s.config.system.build.toplevel", pg),
	)
}
