package role

import "github.com/hanselrd/dotfiles/internal/accesslevel"

type SystemRole uint

//go:generate go run github.com/dmarkham/enumer -type SystemRole -trimprefix SystemRole -linecomment -json -text -transform lower

const (
	SystemRoleBootstrap SystemRole = iota
	SystemRoleBoot
	SystemRoleGaruda
	SystemRoleHomeManager // home-manager
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

func (r SystemRole) AccessLevel() accesslevel.AccessLevel {
	return accesslevel.AccessLevelPublic
}
