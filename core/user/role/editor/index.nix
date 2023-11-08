{
  config,
  lib,
  pkgs,
  profile,
  ...
}: {
  programs.neovim = lib.core.user.mkProgram "neovim" {};

  programs.vscode = lib.core.user.mkProgramIf "vscode" (profile.user == "full") {};

  # home.packages = with pkgs;
  #   lib.modules.mkIf (profile.user == "full") [
  #     code-server
  #   ];

  home.sessionVariables = rec {
    CS_DISABLE_FILE_DOWNLOADS = 1;
    EDITOR = "nvim";
    SUDO_EDITOR = EDITOR;
    VISUAL = EDITOR;
  };
}
