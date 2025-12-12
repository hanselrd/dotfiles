package encryption

import "strings"

//go:generate go run github.com/dmarkham/enumer -type Encryption -trimprefix Encryption
//go:generate go run ../codegen/marshaler.go -type Encryption -stringer NixString

type Encryption uint

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
