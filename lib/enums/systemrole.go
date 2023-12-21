package enums

type SystemRole uint

//go:generate stringer -type SystemRole -linecomment
//go:generate go run codegen.go SystemRole

const (
	SystemRoleBootstrap      SystemRole = iota // bootstrap
	SystemRoleBoot                             // boot
	SystemRoleI18n                             // i18n
	SystemRoleKernel                           // kernel
	SystemRoleMonitoring                       // monitoring
	SystemRoleMotd                             // motd
	SystemRoleNetworking                       // networking
	SystemRoleNix                              // nix
	SystemRoleOpenSsh                          // openssh
	SystemRoleShell                            // shell
	SystemRoleTime                             // time
	SystemRoleUser                             // user
	SystemRoleVirtualization                   // virtualization
	SystemRoleX11                              // x11
)

func (r SystemRole) Type() string {
	return "system"
}

func (r SystemRole) Role() string {
	return r.String()
}
