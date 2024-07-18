package codegen

import (
	"fmt"
	"log/slog"
	"os"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/pkg/role"
)

var rolesCmd = &cobra.Command{
	Use:   "roles",
	Short: "Roles command",
	Long:  "Roles command",
	Run: func(cmd *cobra.Command, args []string) {
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

			f, err := os.Create(fmt.Sprintf("%s/roles.nix", roles[0].Type()))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "roles.nix.gotmpl", roles)
			cobra.CheckErr(err)
		}
	},
}

func init() {
	CodegenCmd.AddCommand(rolesCmd)
}
