package environment

//go:generate go run roles_codegen.go

type (
	environmentRolesSystemBootstrap  struct{}
	environmentRolesSystemBoot       struct{}
	environmentRolesSystemChocolatey struct{}
	environmentRolesSystemCockpit    struct{}
	environmentRolesSystemDocker     struct{}
	environmentRolesSystemFuse       struct{}
	environmentRolesSystemGaruda     struct{}
	environmentRolesSystemGlazeWM    struct{}
	environmentRolesSystemGrub       struct {
		Device string `json:"device"`
	}
	environmentRolesSystemHomeManager struct{}
	environmentRolesSystemHyprland    struct{}
	environmentRolesSystemI18N        struct{}
	environmentRolesSystemKDE         struct{}
	environmentRolesSystemKernel      struct{}
	environmentRolesSystemMonitoring  struct{}
	environmentRolesSystemMotd        struct{}
	environmentRolesSystemNetworking  struct {
		HostName string `json:"hostName"`
	}
	environmentRolesSystemNix         struct{}
	environmentRolesSystemOpenSsh     struct{}
	environmentRolesSystemQemuGuest   struct{}
	environmentRolesSystemShell       struct{}
	environmentRolesSystemSudo        struct{}
	environmentRolesSystemSystemdBoot struct {
		Xbootldr bool `json:"xbootldr"`
	}
	environmentRolesSystemTime    struct{}
	environmentRolesSystemUdisks2 struct{}
	environmentRolesSystemUser    struct{}
	environmentRolesSystemWinGet  struct{}
	environmentRolesSystemWsl     struct{}
	environmentRolesSystemXServer struct{}
	environmentRolesSystemXrdp    struct{}
	environmentRolesSystemZram    struct{}
)

type (
	environmentRolesUserBootstrap             struct{}
	environmentRolesUserAlacritty             struct{}
	environmentRolesUserBash                  struct{}
	environmentRolesUserBat                   struct{}
	environmentRolesUserBin                   struct{}
	environmentRolesUserBottles               struct{}
	environmentRolesUserBrave                 struct{}
	environmentRolesUserBrowser               struct{}
	environmentRolesUserBtop                  struct{}
	environmentRolesUserCMake                 struct{}
	environmentRolesUserCcache                struct{}
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
	environmentRolesUserGaming                struct{}
	environmentRolesUserGdb                   struct{}
	environmentRolesUserGit                   struct{}
	environmentRolesUserHtop                  struct{}
	environmentRolesUserLldb                  struct{}
	environmentRolesUserMisc                  struct{}
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
	environmentRolesUserWine     struct{}
	environmentRolesUserWizTree  struct{}
	environmentRolesUserXdg      struct{}
	environmentRolesUserZoxide   struct{}
	environmentRolesUserZsh      struct{}
)
