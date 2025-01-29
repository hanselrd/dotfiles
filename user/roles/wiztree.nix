{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.roles.user.wiztree;
in
{
  options = {
    roles.user.wiztree = {
      enable = lib.mkEnableOption "roles.user.wiztree";
    };
  };

  config = lib.mkIf cfg.enable {
    home.activation.wiztree0 = lib.mkIf lib.profiles.isSystemWsl (
      lib.common.runExternalOnceHome "wiztree0" ''
        winget.exe install --exact --id --disable-interactivity AntibodySoftware.WizTree
        }
      ''
    );
  };
}
