package encryption

import "strings"

type Encryption uint

//go:generate go run github.com/dmarkham/enumer -type Encryption -trimprefix Encryption
//go:generate go run ../codegen/nixstringee.go Encryption

const (
	EncryptionNone Encryption = iota
	EncryptionDefault
	EncryptionRed
	EncryptionYellow
	EncryptionBlue
)

func (e Encryption) NixString() string {
	return strings.ToLower(e.String())
}
