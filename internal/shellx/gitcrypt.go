package shellx

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/shell"
)

func GitCryptInit(e encryption.Encryption) error {
	var err error
	switch e {
	case encryption.EncryptionNone:
		// do nothing
	case encryption.EncryptionDefault:
		_, err = shell.Shell("git-crypt init")
	default:
		_, err = shell.Shell(fmt.Sprintf("git-crypt init -k %s", e.NixString()))
	}
	return err
}
