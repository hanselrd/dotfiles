package cmd

import (
	"fmt"
	"os"
	"text/template"

	"github.com/iancoleman/strcase"
	"github.com/rs/zerolog/log"
	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/assets"
	"github.com/hanselrd/dotfiles/pkg/enums"
	"github.com/hanselrd/dotfiles/pkg/interfaces"
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
			lo.Flatten([][]interfaces.Profile{
				lo.Map(
					enums.SystemProfileValues(),
					func(p enums.SystemProfile, _ int) interfaces.Profile { return p },
				),
				lo.Map(
					enums.UserProfileValues(),
					func(p enums.UserProfile, _ int) interfaces.Profile { return p },
				),
			}))
		cobra.CheckErr(err)

		for _, role := range enums.SystemRoleValues() {
			if _, err := os.Stat(fmt.Sprintf("system/roles/%s.nix", role)); !os.IsNotExist(err) {
				log.Debug().
					Str("file", fmt.Sprintf("system/roles/%s.nix", role)).
					Msg("skipping, already exists")
				continue
			}

			f, err := os.Create(fmt.Sprintf("system/roles/%s.nix", role))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "role.nix.gotmpl", role)
			cobra.CheckErr(err)
		}

		for _, role := range enums.UserRoleValues() {
			if _, err := os.Stat(fmt.Sprintf("user/roles/%s.nix", role)); !os.IsNotExist(err) {
				log.Debug().
					Str("file", fmt.Sprintf("user/roles/%s.nix", role)).
					Msg("skipping, already exists")
				continue
			}

			f, err := os.Create(fmt.Sprintf("user/roles/%s.nix", role))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "role.nix.gotmpl", role)
			cobra.CheckErr(err)
		}

		f, err = os.Create("system/roles.nix")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(f, "roles.nix.gotmpl", enums.SystemRoleValues())
		cobra.CheckErr(err)

		f, err = os.Create("user/roles.nix")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(f, "roles.nix.gotmpl", enums.UserRoleValues())
		cobra.CheckErr(err)
	},
}

func init() {
	rootCmd.AddCommand(templateCmd)
}
