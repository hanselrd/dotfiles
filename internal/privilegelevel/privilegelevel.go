package privilegelevel

import "strings"

type PrivilegeLevel uint

//go:generate go run github.com/dmarkham/enumer -type PrivilegeLevel -trimprefix PrivilegeLevel
//go:generate go run ../codegen/nixstringee.go PrivilegeLevel

const (
	PrivilegeLevelSystem PrivilegeLevel = iota
	PrivilegeLevelUser
)

func (pl PrivilegeLevel) NixString() string {
	return strings.ToLower(pl.String())
}
