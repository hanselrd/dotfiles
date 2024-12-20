package environment

//go:generate go run roles_codegen.go

type (
	environmentRolesSystemBootstrap   struct{}
	environmentRolesSystemBoot        struct{}
	environmentRolesSystemCockpit     struct{}
	environmentRolesSystemDocker      struct{}
	environmentRolesSystemGaruda      struct{}
	environmentRolesSystemHomeManager struct{}
	environmentRolesSystemHyprland    struct{}
	environmentRolesSystemI18N        struct{}
	environmentRolesSystemKde         struct{}
	environmentRolesSystemKernel      struct{}
	environmentRolesSystemMonitoring  struct{}
	environmentRolesSystemMotd        struct{}
	environmentRolesSystemNetworking  struct {
		HostName string `json:"hostName"`
	}
	environmentRolesSystemNix     struct{}
	environmentRolesSystemOpenssh struct{}
	environmentRolesSystemShell   struct{}
	environmentRolesSystemSudo    struct{}
	environmentRolesSystemTime    struct{}
	environmentRolesSystemUdisks2 struct{}
	environmentRolesSystemUser    struct{}
	environmentRolesSystemWsl     struct{}
	environmentRolesSystemXrdp    struct{}
	environmentRolesSystemXserver struct{}
)

type (
	environmentRolesUserBootstrap             struct{}
	environmentRolesUserAlacritty             struct{}
	environmentRolesUserBash                  struct{}
	environmentRolesUserBat                   struct{}
	environmentRolesUserBrave                 struct{}
	environmentRolesUserBrowser               struct{}
	environmentRolesUserBtop                  struct{}
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
	environmentRolesUserFastfetch             struct{}
	environmentRolesUserFlameshot             struct{}
	environmentRolesUserFonts                 struct{}
	environmentRolesUserFzf                   struct{}
	environmentRolesUserGdb                   struct{}
	environmentRolesUserGit                   struct{}
	environmentRolesUserHtop                  struct{}
	environmentRolesUserLldb                  struct{}
	environmentRolesUserNeovim                struct{}
	environmentRolesUserNix                   struct {
		Sandbox bool `json:"sandbox"`
	}
	environmentRolesUserOhMyPosh struct{}
	environmentRolesUserPager    struct{}
	environmentRolesUserRanger   struct{}
	environmentRolesUserRedshift struct{}
	environmentRolesUserRipgrep  struct{}
	environmentRolesUserRofi     struct{}
	environmentRolesUserRts      struct{}
	environmentRolesUserScripts  struct{}
	environmentRolesUserShell    struct {
		BashToZsh bool `json:"bashToZsh"`
		LdPreload bool `json:"ldPreload"`
		Theme     bool `json:"theme"`
	}
	environmentRolesUserSsh      struct{}
	environmentRolesUserStarship struct{}
	environmentRolesUserSxhkd    struct{}
	environmentRolesUserTerminal struct{}
	environmentRolesUserTheme    struct{}
	environmentRolesUserTmux     struct{}
	environmentRolesUserVscode   struct{}
	environmentRolesUserXdg      struct{}
	environmentRolesUserZoxide   struct{}
	environmentRolesUserZsh      struct{}
)
