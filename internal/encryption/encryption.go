package encryption

type Encryption uint

//go:generate go run github.com/dmarkham/enumer -type Encryption -trimprefix Encryption -linecomment -json -text -transform lower

const (
	EncryptionNone Encryption = iota
	EncryptionDefault
	EncryptionRed
	EncryptionYellow
	EncryptionBlue
)
