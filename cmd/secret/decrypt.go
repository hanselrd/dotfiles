package secret

import (
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"strings"

	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var decryptCmd = &cobra.Command{
	Use:   "decrypt",
	Short: "Decrypt command",
	Long:  "Decrypt command",
	Run: func(cmd *cobra.Command, args []string) {
		files := []string{}
		err := filepath.Walk("internal/assets/secrets",
			func(path string, info fs.FileInfo, err error) error {
				if info.IsDir() || filepath.Ext(path) != ".age" {
					return nil
				}
				slog.Debug("", "path", path)
				files = append(files, path)
				return nil
			})
		cobra.CheckErr(err)

		lop.ForEach(files, func(f string, _ int) {
			split := strings.Split(f, string(os.PathSeparator))
			f = filepath.Join(split[2:]...)
			f = f[:len(f)-len(".age")]
			os.MkdirAll(filepath.Dir(f), 0o755)

			shell.Shell(
				fmt.Sprintf(
					"age -d -i %s/.keys/2.age internal/assets/%[2]s.age > %[2]s",
					environment.Environment.User.HomeDirectory,
					f,
				),
			)
		})
	},
}

func init() {
	SecretCmd.AddCommand(decryptCmd)
}
