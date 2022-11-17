{
  config,
  lib,
  pkgs,
  preset,
  ...
}: {
  programs.neovim = lib.ext.mkProgram "neovim" {};

  programs.vscode = lib.ext.mkProgramIf "vscode" (preset.user == "desktop") {};

  home.sessionVariables = rec {
    EDITOR = "nvim";
    SUDO_EDITOR = EDITOR;
    VISUAL = EDITOR;
  };
}
