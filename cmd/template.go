//go:build !windows

package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"text/template"

	"github.com/iancoleman/strcase"
	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/assets"
	"github.com/hanselrd/dotfiles/pkg/profile"
	"github.com/hanselrd/dotfiles/pkg/role"
)

var templateCmd = &cobra.Command{
	Use:   "template",
	Short: "Template command",
	Long:  "Template command",
	Run: func(cmd *cobra.Command, args []string) {
		tmpl, err := template.New("").Funcs(template.FuncMap{
			"camel":      strcase.ToCamel,
			"lowerCamel": strcase.ToLowerCamel,
		}).ParseFS(assets.TemplatesFS, "templates/*.gotmpl")
		cobra.CheckErr(err)

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

		for _, roles := range [][]role.Role{
			lo.Map(
				role.SystemRoleValues(),
				func(r role.SystemRole, _ int) role.Role { return r },
			),
			lo.Map(
				role.UserRoleValues(),
				func(r role.UserRole, _ int) role.Role { return r },
			),
		} {
			lop.ForEach(roles, func(r role.Role, _ int) {
				if _, err := os.Stat(fmt.Sprintf("%s/roles/%s.nix", r.Type(), r)); !os.IsNotExist(
					err,
				) {
					slog.Debug(
						"skipping, already exists",
						"file",
						fmt.Sprintf("%s/roles/%s.nix", r.Type(), r),
					)
					return
				}

				f, err := os.Create(fmt.Sprintf("%s/roles/%s.nix", r.Type(), r))
				cobra.CheckErr(err)
				err = tmpl.ExecuteTemplate(f, "role.nix.gotmpl", r)
				cobra.CheckErr(err)
			})

			f, err = os.Create(fmt.Sprintf("%s/roles.nix", roles[0].Type()))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "roles.nix.gotmpl", roles)
			cobra.CheckErr(err)
		}
	},
}

func init() {
	rootCmd.AddCommand(templateCmd)
}
