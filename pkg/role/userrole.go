package role

import "github.com/hanselrd/dotfiles/internal/accesslevel"

type UserRole uint

//go:generate go run github.com/dmarkham/enumer -type UserRole -trimprefix UserRole -linecomment -json -text -transform lower

const (
	UserRoleBootstrap UserRole = iota
	UserRoleAlacritty
	UserRoleBash
	UserRoleBat
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
	UserRoleZoxide
	UserRoleZsh
)

func (r UserRole) Type() string {
	return "user"
}

func (r UserRole) AccessLevel() accesslevel.AccessLevel {
	switch r {
	case UserRoleRts:
		return accesslevel.AccessLevelPrivate
	}
	return accesslevel.AccessLevelPublic
}
