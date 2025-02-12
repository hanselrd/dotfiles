package role

import (
	"strings"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type UserRole uint

//go:generate go run github.com/dmarkham/enumer -type UserRole -trimprefix UserRole -linecomment -json -text

const (
	UserRoleBootstrap UserRole = iota
	UserRoleAlacritty
	UserRoleBash
	UserRoleBat
	UserRoleBin
	UserRoleBrave
	UserRoleBrowser
	UserRoleBtop
	UserRoleDevelopment
	UserRoleDevelopmentCpp
	UserRoleDevelopmentDhall
	UserRoleDevelopmentElixir
	UserRoleDevelopmentGleam
	UserRoleDevelopmentGo
	UserRoleDevelopmentHaskell
	UserRoleDevelopmentJava
	UserRoleDevelopmentKotlin
	UserRoleDevelopmentLua
	UserRoleDevelopmentNickel
	UserRoleDevelopmentNix
	UserRoleDevelopmentNodejs
	UserRoleDevelopmentPurescript
	UserRoleDevelopmentPython
	UserRoleDevelopmentRust
	UserRoleDevelopmentShell
	UserRoleDevelopmentZig
	UserRoleDocker
	UserRoleEditor
	UserRoleEza
	UserRoleFastfetch
	UserRoleFlameshot
	UserRoleFonts
	UserRoleFzf
	UserRoleGdb
	UserRoleGit
	UserRoleHtop
	UserRoleLldb
	UserRoleNeovim
	UserRoleNix
	UserRoleOhMyPosh
	UserRolePager
	UserRoleRanger
	UserRoleRedshift
	UserRoleRipgrep
	UserRoleRofi
	UserRoleRts
	UserRoleShell
	UserRoleSsh
	UserRoleStarship
	UserRoleSxhkd
	UserRoleTerminal
	UserRoleTheme
	UserRoleTmux
	UserRoleVscode
	UserRoleWizTree
	UserRoleXdg
	UserRoleZoxide
	UserRoleZsh
)

func (r UserRole) NixString() string {
	switch r {
	case UserRoleDevelopmentCpp:
		fallthrough
	case UserRoleDevelopmentDhall:
		fallthrough
	case UserRoleDevelopmentElixir:
		fallthrough
	case UserRoleDevelopmentGleam:
		fallthrough
	case UserRoleDevelopmentGo:
		fallthrough
	case UserRoleDevelopmentHaskell:
		fallthrough
	case UserRoleDevelopmentJava:
		fallthrough
	case UserRoleDevelopmentKotlin:
		fallthrough
	case UserRoleDevelopmentLua:
		fallthrough
	case UserRoleDevelopmentNickel:
		fallthrough
	case UserRoleDevelopmentNix:
		fallthrough
	case UserRoleDevelopmentNodejs:
		fallthrough
	case UserRoleDevelopmentPurescript:
		fallthrough
	case UserRoleDevelopmentPython:
		fallthrough
	case UserRoleDevelopmentRust:
		fallthrough
	case UserRoleDevelopmentShell:
		fallthrough
	case UserRoleDevelopmentZig:
		return strings.ToLower(strings.Join(lo.Words(r.String()), "."))
	case UserRoleOhMyPosh:
		return strings.ToLower(strings.Join(lo.Words(r.String()), "-"))
	}
	return strings.ToLower(r.String())
}

func (r UserRole) PrivilegeLevel() privilegelevel.PrivilegeLevel {
	return privilegelevel.PrivilegeLevelUser
}

func (r UserRole) Enabled() bool {
	return true
}

func (r UserRole) Encryption() encryption.Encryption {
	switch r {
	case UserRoleRts:
		return encryption.EncryptionYellow
	}
	return encryption.EncryptionNone
}
