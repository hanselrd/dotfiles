package accesslevel

type AccessLevel uint

//go:generate go run github.com/dmarkham/enumer -type AccessLevel -trimprefix AccessLevel -linecomment -json -text -transform lower

const (
	AccessLevelDisabled AccessLevel = iota
	AccessLevelPublic
	AccessLevelSecret
)
