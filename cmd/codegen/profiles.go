package codegen

import (
	"fmt"
	"log/slog"
	"os"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

var profilesCmd = &cobra.Command{
	Use:   "profiles",
	Short: "Profiles command",
	Long:  "Profiles command",
	Run: func(cmd *cobra.Command, args []string) {
		for _, profiles := range [][]profile.Profile{
			lo.Map(
				profile.SystemProfileValues(),
				func(r profile.SystemProfile, _ int) profile.Profile { return r },
			),
			lo.Map(
				profile.UserProfileValues(),
				func(r profile.UserProfile, _ int) profile.Profile { return r },
			),
		} {
			lop.ForEach(profiles, func(r profile.Profile, _ int) {
				if _, err := os.Stat(fmt.Sprintf("%s/profiles/%s.nix", r.Type(), r)); !os.IsNotExist(
					err,
				) {
					slog.Debug(
						"skipping, already exists",
						"file",
						fmt.Sprintf("%s/profiles/%s.nix", r.Type(), r),
					)
					return
				}

				f, err := os.Create(fmt.Sprintf("%s/profiles/%s.nix", r.Type(), r))
				cobra.CheckErr(err)
				err = tmpl.ExecuteTemplate(f, "profile.nix.gotmpl", r)
				cobra.CheckErr(err)
			})
		}

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
