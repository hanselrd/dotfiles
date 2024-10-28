package codegen

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/shell"
)

var encryptionCmd = &cobra.Command{
	Use:   "encryption",
	Short: "Encryption command",
	Long:  "Encryption command",
	Run: func(cmd *cobra.Command, args []string) {
		for _, e := range lo.DropByIndex(encryption.EncryptionValues(), 0) {
			file := ".encrypted"
			switch e {
			case encryption.EncryptionNone:
				// do nothing
			case encryption.EncryptionDefault:
				file = filepath.Join("secrets", file)
			default:
				file = filepath.Join(fmt.Sprintf("secrets/%s", e), file)
			}
			if _, err := os.Stat(file); !os.IsNotExist(err) {
				continue
			}
			switch e {
			case encryption.EncryptionNone:
				// do nothing
			case encryption.EncryptionDefault:
				shell.Shell("git-crypt init")
			default:
				shell.Shell(fmt.Sprintf("git-crypt init -k %s", e))
			}
			os.MkdirAll(filepath.Dir(file), 0o755)
			os.WriteFile(file, []byte("false"), 0o644)
		}
		f, err := os.Create(".gitattributes")
		cobra.CheckErr(err)
		err = tmpl.ExecuteTemplate(
			f,
			"gitattributes.gotmpl",
			lo.DropByIndex(encryption.EncryptionValues(), 0),
		)
		cobra.CheckErr(err)
	},
}

func init() {
	CodegenCmd.AddCommand(encryptionCmd)
}
