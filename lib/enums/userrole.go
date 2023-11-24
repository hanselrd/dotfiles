package enums

type UserRole uint

//go:generate stringer -type UserRole -linecomment
//go:generate go run codegen.go UserRole

const (
	UserRoleBrowser     UserRole = iota // browser
	UserRoleDevelopment                 // development
	UserRoleDocker                      // docker
	UserRoleEditor                      // editor
	UserRoleHomeage                     // homeage
	UserRoleNix                         // nix
	UserRolePager                       // pager
	UserRoleShell                       // shell
	UserRoleTerminal                    // terminal
	UserRoleTheme                       // theme
)
