package secret

import (
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"

	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
)

var encryptCmd = &cobra.Command{
	Use:   "encrypt",
	Short: "Encrypt command",
	Long:  "Encrypt command",
	Run: func(cmd *cobra.Command, args []string) {
		files := []string{}
		err := filepath.Walk("secrets",
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
			os.MkdirAll(filepath.Dir(fmt.Sprintf("internal/assets/%s", f)), 0o755)

			shell.Shell(
				fmt.Sprintf(
					"age -a -R internal/assets/keys/1.age.pub %[1]s > internal/assets/%[1]s.age",
					f,
				),
			)
		})
	},
}

func init() {
	SecretCmd.AddCommand(encryptCmd)
}
