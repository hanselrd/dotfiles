package enums

type SystemRole uint

//go:generate stringer -type SystemRole -linecomment
//go:generate go run codegen.go SystemRole

const (
	SystemRoleBoot           SystemRole = iota // boot
	SystemRoleI18n                             // i18n
	SystemRoleMonitoring                       // monitoring
	SystemRoleMotd                             // motd
	SystemRoleNetworking                       // networking
	SystemRoleNix                              // nix
	SystemRoleShell                            // shell
	SystemRoleTime                             // time
	SystemRoleUser                             // user
	SystemRoleVirtualization                   // virtualization
	SystemRoleX11                              // x11
)
