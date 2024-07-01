package homeage

import (
	"fmt"
	"io/fs"
	"log/slog"
	"path/filepath"

	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
)

var secretCmd = &cobra.Command{
	Use:   "secret",
	Short: "Secret command",
	Long:  "Secret command",
	Run: func(cmd *cobra.Command, args []string) {
		files := []string{}
		err := filepath.Walk("user/roles/homeage/secrets",
			func(path string, info fs.FileInfo, err error) error {
				if info.IsDir() || filepath.Ext(path) == ".age" {
					return nil
				}
				slog.Debug("", "path", path)
				files = append(files, path)
				return nil
			})
		cobra.CheckErr(err)

		lop.ForEach(files, func(f string, _ int) {
			shell.Shell(
				fmt.Sprintf("age -a -R user/roles/homeage/keys/1.age.pub %[1]s > %[1]s.age", f),
			)
		})
	},
}

func init() {
	HomeageCmd.AddCommand(secretCmd)
}
