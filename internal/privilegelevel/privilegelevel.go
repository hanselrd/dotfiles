package privilegelevel

type PrivilegeLevel uint

//go:generate go run github.com/dmarkham/enumer -type PrivilegeLevel -trimprefix PrivilegeLevel -linecomment -json -text -transform lower

const (
	PrivilegeLevelSystem PrivilegeLevel = iota
	PrivilegeLevelUser
)
