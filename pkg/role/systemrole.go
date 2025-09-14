package role

import (
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type SystemRole uint

//go:generate go run github.com/dmarkham/enumer -type SystemRole -trimprefix SystemRole
//go:generate go run ../../internal/codegen/nixstringee.go SystemRole

const (
	SystemRoleBootstrap SystemRole = iota
	SystemRoleBoot
	SystemRoleChocolatey
	SystemRoleCockpit
	SystemRoleDocker
	SystemRoleFuse
	SystemRoleGaruda
	SystemRoleGlazeWM
	SystemRoleGrub
	SystemRoleHomeManager
	SystemRoleHyprland
	SystemRoleKDE
	SystemRoleKernel
	SystemRoleLanguage
	SystemRoleMonitoring
	SystemRoleMotd
	SystemRoleNetworking
	SystemRoleNix
	SystemRoleOpenSsh
	SystemRoleQemuGuest
	SystemRoleShell
	SystemRoleSudo
	SystemRoleSystemdBoot
	SystemRoleTime
	SystemRoleUdisks2
	SystemRoleUser
	SystemRoleWinGet
	SystemRoleWsl
	SystemRoleXServer
	SystemRoleXrdp
	SystemRoleZram
)

func (r SystemRole) NixString() string {
	switch r {
	case SystemRoleHomeManager, SystemRoleSystemdBoot:
		return strings.ToLower(strings.Join(lo.Words(r.String()), "-"))
	}
	return strings.ToLower(r.String())
}

func (r SystemRole) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelSystem
}

func (r SystemRole) Enabled() bool {
	return true
}

func (r SystemRole) Encryption() encryption.Encryption {
	return encryption.EncryptionNone
}
