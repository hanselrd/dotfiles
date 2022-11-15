{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.neovim = lib.ext.mkProgram "neovim" {};

  # TODO: add way to specify "supportedPresets"
  # programs.vscode = lib.ext.mkProgram "vscode" {};

  home.sessionVariables = rec {
    EDITOR = "nvim";
    SUDO_EDITOR = EDITOR;
    VISUAL = EDITOR;
  };
}
