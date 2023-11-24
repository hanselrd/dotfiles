package cmd

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"text/template"

	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/enums"
)

var templateCmd = &cobra.Command{
	Use:   "template",
	Short: "Template command",
	Long:  "Template command",
	Run: func(cmd *cobra.Command, args []string) {
		tmpl := template.New("")

		err := filepath.Walk("templates", func(path string, info fs.FileInfo, err error) error {
			if strings.HasSuffix(path, ".gotmpl") {
				_, err := tmpl.ParseFiles(path)
				if err != nil {
					log.Error().Err(err).Send()
					return err
				}
			}
			return nil
		})
		cobra.CheckErr(err)

		for _, role := range enums.SystemRoles() {
			if _, err := os.Stat(fmt.Sprintf("system/roles/%s.nix", role)); !os.IsNotExist(err) {
				log.Debug().Str("file", fmt.Sprintf("system/roles/%s.nix", role)).Msg("skipping, already exists")
				continue
			}

			data := map[string]any{
				"type": "system",
				"role": role,
			}

			f, err := os.Create(fmt.Sprintf("system/roles/%s.nix", role))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "role.nix", data)
			cobra.CheckErr(err)
		}

		for _, role := range enums.UserRoles() {
			if _, err := os.Stat(fmt.Sprintf("user/roles/%s.nix", role)); !os.IsNotExist(err) {
				log.Debug().Str("file", fmt.Sprintf("user/roles/%s.nix", role)).Msg("skipping, already exists")
				continue
			}

			data := map[string]any{
				"type": "user",
				"role": role,
			}

			f, err := os.Create(fmt.Sprintf("user/roles/%s.nix", role))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "role.nix", data)
			cobra.CheckErr(err)
		}

		f, err := os.Create("system/roles.nix")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(f, "roles.nix", enums.SystemRoles())
		cobra.CheckErr(err)

		f, err = os.Create("user/roles.nix")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(f, "roles.nix", enums.UserRoles())
		cobra.CheckErr(err)
	},
}

func init() {
	rootCmd.AddCommand(templateCmd)
}
