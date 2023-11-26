package enums

type UserRole uint

//go:generate stringer -type UserRole -linecomment
//go:generate go run codegen.go UserRole

const (
	UserRoleAlacritty             UserRole = iota // alacritty
	UserRoleBash                                  // bash
	UserRoleBat                                   // bat
	UserRoleBootstrap                             // bootstrap
	UserRoleBrowser                               // browser
	UserRoleDevelopment                           // development
	UserRoleDevelopmentCpp                        // development.cpp
	UserRoleDevelopmentDhall                      // development.dhall
	UserRoleDevelopmentElixir                     // development.elixir
	UserRoleDevelopmentGo                         // development.go
	UserRoleDevelopmentHaskell                    // development.haskell
	UserRoleDevelopmentJava                       // development.java
	UserRoleDevelopmentLua                        // development.lua
	UserRoleDevelopmentNickel                     // development.nickel
	UserRoleDevelopmentNix                        // development.nix
	UserRoleDevelopmentNodejs                     // development.nodejs
	UserRoleDevelopmentPurescript                 // development.purescript
	UserRoleDevelopmentPython                     // development.python
	UserRoleDevelopmentRust                       // development.rust
	UserRoleDevelopmentShell                      // development.shell
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
	UserRoleScripts                               // scripts
	UserRoleShell                                 // shell
	UserRoleSsh                                   // ssh
	UserRoleStarship                              // starship
	UserRoleTerminal                              // terminal
	UserRoleTheme                                 // theme
	UserRoleTmux                                  // tmux
	UserRoleVscode                                // vscode
	UserRoleZsh                                   // zsh
	UserRoleZzz                                   // zzz
)
