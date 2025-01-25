package codegen

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/asset"
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
				func(p profile.SystemProfile, _ int) profile.Profile { return p },
			),
			lo.Map(
				profile.UserProfileValues(),
				func(p profile.UserProfile, _ int) profile.Profile { return p },
			),
		} {
			lop.ForEach(profiles, func(p profile.Profile, _ int) {
				file := fmt.Sprintf("%s/profiles/%s.nix", p.PrivilegeLevel(), p)
				os.MkdirAll(filepath.Dir(file), 0o755)

				if _, err := os.Stat(file); !os.IsNotExist(err) {
					slog.Debug(
						"skipping, already exists",
						"file",
						file,
					)
					return
				}

				f, err := os.Create(file)
				cobra.CheckErr(err)
				err = asset.Template.ExecuteTemplate(f, "profile.nix.gotmpl", p)
				cobra.CheckErr(err)
			})
		}

		f, err := os.Create("lib/profiles.nix")
		cobra.CheckErr(err)
		err = asset.Template.ExecuteTemplate(f, "profiles.nix.gotmpl",
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
