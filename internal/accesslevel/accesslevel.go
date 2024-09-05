package accesslevel

type AccessLevel uint

const (
	AccessLevelDisabled AccessLevel = iota
	AccessLevelPublic
	AccessLevelPrivate
)
