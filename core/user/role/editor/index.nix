{
  config,
  lib,
  pkgs,
  preset,
  ...
}: {
  programs.neovim = lib.core.user.mkProgram "neovim" {};

  programs.vscode = lib.core.user.mkProgramIf "vscode" (preset.user == "desktop") {};

  home.packages = with pkgs;
    lib.modules.mkIf (preset.user == "desktop") [
      code-server
    ];

  home.sessionVariables = rec {
    EDITOR = "nvim";
    SUDO_EDITOR = EDITOR;
    VISUAL = EDITOR;
  };
}
