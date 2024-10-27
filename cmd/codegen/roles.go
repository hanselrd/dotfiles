package codegen

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/accesslevel"
	"github.com/hanselrd/dotfiles/internal/encryption"
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
				file := fmt.Sprintf("%s/roles/%s.nix", r.PrivilegeLevel(), r)
				switch r.AccessLevel() {
				case accesslevel.AccessLevelDisabled:
					return
				case accesslevel.AccessLevelSecret:
					switch r.Encryption() {
					case encryption.EncryptionDefault:
						file = filepath.Join("secrets", file)
					case encryption.EncryptionPrivate:
						file = filepath.Join("secrets/private", file)
					}
				}
				os.MkdirAll(filepath.Dir(file), 0o755)

				if _, err := os.Stat(file); !os.IsNotExist(
					err,
				) {
					slog.Debug(
						"skipping, already exists",
						"file",
						file,
					)
					return
				}

				f, err := os.Create(file)
				cobra.CheckErr(err)
				err = tmpl.ExecuteTemplate(f, "role.nix.gotmpl", r)
				cobra.CheckErr(err)
			})

			f, err := os.Create(fmt.Sprintf("%s/roles.nix", roles[0].PrivilegeLevel()))
			cobra.CheckErr(err)
			err = tmpl.ExecuteTemplate(f, "roles.nix.gotmpl", roles)
			cobra.CheckErr(err)
		}
	},
}

func init() {
	CodegenCmd.AddCommand(rolesCmd)
}
