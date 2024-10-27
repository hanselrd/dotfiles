package role

import (
	"github.com/hanselrd/dotfiles/internal/accesslevel"
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

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
	SystemRoleUdisks2
	SystemRoleUser
	SystemRoleWsl
	SystemRoleXServer
	SystemRoleXrdp
)

func (r SystemRole) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelSystem
}

func (r SystemRole) AccessLevel() accesslevel.AccessLevel {
	return accesslevel.AccessLevelPublic
}

func (r SystemRole) Encryption() encryption.Encryption {
	return encryption.EncryptionNone
}
