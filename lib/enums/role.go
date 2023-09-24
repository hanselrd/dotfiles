package enums

import "encoding/json"

type Role uint

//go:generate stringer -type Role -linecomment

const (
	RoleBrowser     Role = iota // browser
	RoleCommon                  // common
	RoleDevelopment             // development
	RoleDocker                  // docker
	RoleEditor                  // editor
	RoleHomeage                 // homeage
	RoleNix                     // nix
	RoleOther                   // other
	RolePager                   // pager
	RoleScripts                 // scripts
	RoleShell                   // shell
	RoleTerminal                // terminal
	RoleTheme                   // theme
)

func (r Role) MarshalText() ([]byte, error) {
	return []byte(r.String()), nil
}

func (r Role) MarshalJSON() ([]byte, error) {
	return json.Marshal(r.String())
}
