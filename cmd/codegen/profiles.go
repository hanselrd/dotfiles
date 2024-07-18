package codegen

import (
	"os"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

var profilesCmd = &cobra.Command{
	Use:   "profiles",
	Short: "Profiles command",
	Long:  "Profiles command",
	Run: func(cmd *cobra.Command, args []string) {
		f, err := os.Create("lib/profiles.nix")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(f, "profiles.nix.gotmpl",
			lo.Flatten([][]profile.Profile{
				lo.Map(
					profile.SystemProfileValues(),
					func(p profile.SystemProfile, _ int) profile.Profile { return p },
				),
				lo.Map(
					profile.UserProfileValues(),
					func(p profile.UserProfile, _ int) profile.Profile { return p },
				),
			}))
		cobra.CheckErr(err)
	},
}

func init() {
	CodegenCmd.AddCommand(profilesCmd)
}
