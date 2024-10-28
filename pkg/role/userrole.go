package role

import (
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
)

type UserRole uint

//go:generate go run github.com/dmarkham/enumer -type UserRole -trimprefix UserRole -linecomment -json -text -transform lower

const (
	UserRoleBootstrap UserRole = iota
	UserRoleAlacritty
	UserRoleBash
	UserRoleBat
	UserRoleBrave
	UserRoleBrowser
	UserRoleBtop
	UserRoleDevelopment
	UserRoleDevelopmentCpp        // development.cpp
	UserRoleDevelopmentDhall      // development.dhall
	UserRoleDevelopmentElixir     // development.elixir
	UserRoleDevelopmentGleam      // development.gleam
	UserRoleDevelopmentGo         // development.go
	UserRoleDevelopmentHaskell    // development.haskell
	UserRoleDevelopmentJava       // development.java
	UserRoleDevelopmentKotlin     // development.kotlin
	UserRoleDevelopmentLua        // development.lua
	UserRoleDevelopmentNickel     // development.nickel
	UserRoleDevelopmentNix        // development.nix
	UserRoleDevelopmentNodejs     // development.nodejs
	UserRoleDevelopmentPurescript // development.purescript
	UserRoleDevelopmentPython     // development.python
	UserRoleDevelopmentRust       // development.rust
	UserRoleDevelopmentShell      // development.shell
	UserRoleDevelopmentZig        // development.zig
	UserRoleDocker
	UserRoleEditor
	UserRoleEza
	UserRoleFonts
	UserRoleFzf
	UserRoleGdb
	UserRoleGit
	UserRoleHtop
	UserRoleNeovim
	UserRoleNix
	UserRoleOhMyPosh // oh-my-posh
	UserRolePager
	UserRoleRanger
	UserRoleRipgrep
	UserRoleRts
	UserRoleScripts
	UserRoleShell
	UserRoleSsh
	UserRoleStarship
	UserRoleTerminal
	UserRoleTheme
	UserRoleTmux
	UserRoleVscode
	UserRoleXdg
	UserRoleZoxide
	UserRoleZsh
)

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
