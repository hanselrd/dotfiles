package role

type SystemRole uint

//go:generate go run github.com/dmarkham/enumer -type SystemRole -trimprefix SystemRole -linecomment -json -text -transform lower

const (
	SystemRoleBootstrap SystemRole = iota
	SystemRoleBoot
	SystemRoleGaruda
	SystemRoleI18N
	SystemRoleKernel
	SystemRoleMonitoring
	SystemRoleMotd
	SystemRoleNetworking
	SystemRoleNix
	SystemRoleOpenSsh
	SystemRoleShell
	SystemRoleTime
	SystemRoleUser
	SystemRoleVirtualization
	SystemRoleWsl
	SystemRoleX11
)

func (r SystemRole) Type() string {
	return "system"
}
