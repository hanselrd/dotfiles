{
  config,
  lib,
  pkgs,
  env,
  profile,
  ...
}: let
  cfg = config.roles.user.homeage;
in {
  options = {
    roles.user.homeage = {
      enable = lib.mkEnableOption "roles.user.homeage";
    };
  };

  config = lib.mkIf cfg.enable {
    homeage =
      lib.mkIf env.roles.user.homeage.decrypt
      (lib.mkMerge [
        (lib.mkIf
          (profile.system != "nixos" && profile.system != "linux-systemd") {
            mount = "${env.user.cacheDirectory}/nix/homeage/secrets";
          })
        {
          identityPaths = [
            # "${config.home.homeDirectory}/.ssh/id_ed25519"
            # "${config.home.homeDirectory}/.ssh/id_rsa"
            "${config.home.homeDirectory}/.keys/2.age"
          ];

          installationType = "activation";

          file."rts" = {
            source = ./homeage/secrets/rts.sh.age;
            copies = ["${config.home.homeDirectory}/.secrets/rts.sh"];
          };
        }
      ]);
  };
}
