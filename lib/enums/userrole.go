package enums

type UserRole uint

//go:generate stringer -type UserRole -linecomment
//go:generate go run codegen.go UserRole

const (
	UserRoleBootstrap             UserRole = iota // bootstrap
	UserRoleAlacritty                             // alacritty
	UserRoleBash                                  // bash
	UserRoleBat                                   // bat
	UserRoleBrowser                               // browser
	UserRoleDevelopment                           // development
	UserRoleDevelopmentCpp                        // development.cpp
	UserRoleDevelopmentDhall                      // development.dhall
	UserRoleDevelopmentElixir                     // development.elixir
	UserRoleDevelopmentGo                         // development.go
	UserRoleDevelopmentHaskell                    // development.haskell
	UserRoleDevelopmentJava                       // development.java
	UserRoleDevelopmentKotlin                     // development.kotlin
	UserRoleDevelopmentLua                        // development.lua
	UserRoleDevelopmentNickel                     // development.nickel
	UserRoleDevelopmentNix                        // development.nix
	UserRoleDevelopmentNodejs                     // development.nodejs
	UserRoleDevelopmentPurescript                 // development.purescript
	UserRoleDevelopmentPython                     // development.python
	UserRoleDevelopmentRust                       // development.rust
	UserRoleDevelopmentShell                      // development.shell
	UserRoleDevelopmentZig                        // development.zig
	UserRoleDocker                                // docker
	UserRoleEditor                                // editor
	UserRoleEza                                   // eza
	UserRoleFzf                                   // fzf
	UserRoleGit                                   // git
	UserRoleHomeage                               // homeage
	UserRoleHtop                                  // htop
	UserRoleNeovim                                // neovim
	UserRoleNix                                   // nix
	UserRolePager                                 // pager
	UserRoleRipgrep                               // ripgrep
	UserRoleScripts                               // scripts
	UserRoleShell                                 // shell
	UserRoleSsh                                   // ssh
	UserRoleStarship                              // starship
	UserRoleTerminal                              // terminal
	UserRoleTheme                                 // theme
	UserRoleTmux                                  // tmux
	UserRoleVscode                                // vscode
	UserRoleZoxide                                // zoxide
	UserRoleZsh                                   // zsh
	UserRoleZzz                                   // zzz
)

func (r UserRole) Type() string {
	return "user"
}

func (r UserRole) Role() string {
	return r.String()
}
