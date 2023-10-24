package enums

type Role uint

//go:generate stringer -type Role -linecomment
//go:generate go run codegen.go Role

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
