package role

import "github.com/hanselrd/dotfiles/internal/accesslevel"

type SystemRole uint

//go:generate go run github.com/dmarkham/enumer -type SystemRole -trimprefix SystemRole -linecomment -json -text -transform lower

const (
	SystemRoleBootstrap SystemRole = iota
	SystemRoleBoot
	SystemRoleCockpit
	SystemRoleDocker
	SystemRoleGaruda
	SystemRoleHomeManager // home-manager
	SystemRoleI18N
	SystemRoleKDE
	SystemRoleKernel
	SystemRoleMonitoring
	SystemRoleMotd
	SystemRoleNetworking
	SystemRoleNix
	SystemRoleOpenSsh
	SystemRoleShell
	SystemRoleSudo
	SystemRoleTime
	SystemRoleUser
	SystemRoleWsl
	SystemRoleXServer
	SystemRoleXrdp
)

func (r SystemRole) Type() string {
	return "system"
}

func (r SystemRole) AccessLevel() accesslevel.AccessLevel {
	return accesslevel.AccessLevelPublic
}
