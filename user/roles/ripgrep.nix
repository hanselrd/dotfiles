{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.ripgrep;
in {
  options = {
    roles.user.ripgrep = {
      enable = lib.mkEnableOption "roles.user.ripgrep";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ripgrep = {
      enable = true;
      arguments = [
        "--max-columns-preview"
        "--colors=line:style:bold"
      ];
    };
  };
}
