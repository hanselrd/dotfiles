package environment

//go:generate go run roles_codegen.go

type (
	environmentRolesSystemBootstrap  struct{}
	environmentRolesSystemBoot       struct{}
	environmentRolesSystemGaruda     struct{}
	environmentRolesSystemI18N       struct{}
	environmentRolesSystemKernel     struct{}
	environmentRolesSystemMonitoring struct{}
	environmentRolesSystemMotd       struct{}
	environmentRolesSystemNetworking struct {
		HostName string `json:"hostName"`
	}
	environmentRolesSystemNix            struct{}
	environmentRolesSystemOpenssh        struct{}
	environmentRolesSystemShell          struct{}
	environmentRolesSystemTime           struct{}
	environmentRolesSystemUser           struct{}
	environmentRolesSystemVirtualization struct{}
	environmentRolesSystemWsl            struct{}
	environmentRolesSystemX11            struct{}
)

type (
	environmentRolesUserBootstrap             struct{}
	environmentRolesUserAlacritty             struct{}
	environmentRolesUserBash                  struct{}
	environmentRolesUserBat                   struct{}
	environmentRolesUserBrowser               struct{}
	environmentRolesUserDevelopment           struct{}
	environmentRolesUserDevelopmentCpp        struct{}
	environmentRolesUserDevelopmentDhall      struct{}
	environmentRolesUserDevelopmentElixir     struct{}
	environmentRolesUserDevelopmentGleam      struct{}
	environmentRolesUserDevelopmentGo         struct{}
	environmentRolesUserDevelopmentHaskell    struct{}
	environmentRolesUserDevelopmentJava       struct{}
	environmentRolesUserDevelopmentKotlin     struct{}
	environmentRolesUserDevelopmentLua        struct{}
	environmentRolesUserDevelopmentNickel     struct{}
	environmentRolesUserDevelopmentNix        struct{}
	environmentRolesUserDevelopmentNodejs     struct{}
	environmentRolesUserDevelopmentPurescript struct{}
	environmentRolesUserDevelopmentPython     struct{}
	environmentRolesUserDevelopmentRust       struct{}
	environmentRolesUserDevelopmentShell      struct{}
	environmentRolesUserDevelopmentZig        struct{}
	environmentRolesUserDocker                struct{}
	environmentRolesUserEditor                struct{}
	environmentRolesUserEza                   struct{}
	environmentRolesUserFzf                   struct{}
	environmentRolesUserGit                   struct{}
	environmentRolesUserHomeage               struct {
		Decrypt bool `json:"decrypt"`
	}
	environmentRolesUserHtop   struct{}
	environmentRolesUserNeovim struct{}
	environmentRolesUserNix    struct {
		Sandbox bool `json:"sandbox"`
	}
	environmentRolesUserOhMyPosh struct{}
	environmentRolesUserPager    struct{}
	environmentRolesUserRipgrep  struct{}
	environmentRolesUserScripts  struct{}
	environmentRolesUserShell    struct {
		BashToZsh bool `json:"bashToZsh"`
		LdPreload bool `json:"ldPreload"`
		Rts       bool `json:"rts"`
		Theme     bool `json:"theme"`
	}
	environmentRolesUserSsh      struct{}
	environmentRolesUserStarship struct{}
	environmentRolesUserTerminal struct{}
	environmentRolesUserTheme    struct{}
	environmentRolesUserTmux     struct{}
	environmentRolesUserVscode   struct{}
	environmentRolesUserZoxide   struct{}
	environmentRolesUserZsh      struct{}
	environmentRolesUserZzz      struct{}
)
