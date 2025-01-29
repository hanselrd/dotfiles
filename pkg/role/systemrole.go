package role

import (
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type SystemRole uint

//go:generate go run github.com/dmarkham/enumer -type SystemRole -trimprefix SystemRole -linecomment -json -text -transform lower

const (
	SystemRoleBootstrap SystemRole = iota
	SystemRoleBoot
	SystemRoleChocolatey
	SystemRoleCockpit
	SystemRoleDocker
	SystemRoleGaruda
	SystemRoleGlazeWM
	SystemRoleHomeManager // home-manager
	SystemRoleHyprland
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
	SystemRoleUdisks2
	SystemRoleUser
	SystemRoleWinGet
	SystemRoleWsl
	SystemRoleXServer
	SystemRoleXrdp
)

func (r SystemRole) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelSystem
}

func (r SystemRole) Enabled() bool {
	return true
}

func (r SystemRole) Encryption() encryption.Encryption {
	return encryption.EncryptionNone
}
