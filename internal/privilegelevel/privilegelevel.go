package privilegelevel

import "strings"

//go:generate go run github.com/dmarkham/enumer -type PrivilegeLevel -trimprefix PrivilegeLevel
//go:generate go run ../codegen/marshaler.go -type PrivilegeLevel -stringer NixString

type PrivilegeLevel uint

const (
	PrivilegeLevelSystem PrivilegeLevel = iota
	PrivilegeLevelUser
)

func (pl PrivilegeLevel) NixString() string {
	return strings.ToLower(pl.String())
}
