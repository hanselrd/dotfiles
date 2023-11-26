{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.editor;
in {
  options = {
    roles.user.editor = {
      enable = lib.mkEnableOption "roles.user.editor";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.neovim.enable = true;
    # roles.user.vscode.enable = true;

    home.sessionVariables = rec {
      CS_DISABLE_FILE_DOWNLOADS = 1;
      EDITOR = "nvim";
      SUDO_EDITOR = EDITOR;
      VISUAL = EDITOR;
    };
  };
}
