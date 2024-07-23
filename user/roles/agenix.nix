{
  inputs,
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.agenix;
  inherit (inputs) agenix;
in {
  options = {
    roles.user.agenix = {
      enable = lib.mkEnableOption "roles.user.agenix";
    };
  };

  config = lib.mkIf cfg.enable {
    # homeage =
    #   lib.mkIf env.roles.user.homeage.decrypt
    #   (lib.mkMerge [
    #     (lib.mkIf (!env.extra.withSystemd) {
    #       mount = "${env.user.cacheDirectory}/nix/homeage/secrets";
    #     })
    #     {
    #       identityPaths = [
    #         # "${config.home.homeDirectory}/.ssh/id_ed25519"
    #         # "${config.home.homeDirectory}/.ssh/id_rsa"
    #         "${config.home.homeDirectory}/.keys/2.age"
    #       ];

    #       installationType = "activation";

    #       file."rts" = {
    #         source = ./homeage/secrets/rts.sh.age;
    #         copies = ["${config.home.homeDirectory}/.secrets/rts.sh"];
    #       };
    #     }
    #   ]);
    age = lib.mkIf env.roles.user.agenix.decrypt {
      secretsMountPoint = "${env.user.cacheDirectory}/nix/agenix/secrets";
      secretsDir = "${config.home.homeDirectory}/.secrets";

      identityPaths = [
        # "${config.home.homeDirectory}/.ssh/id_ed25519"
        # "${config.home.homeDirectory}/.ssh/id_rsa"
        "${config.home.homeDirectory}/.keys/2.age"
      ];

      secrets."rts.sh" = {
        file = ./agenix/secrets/rts.sh.age;
        symlink = false;
      };
    };

    home.packages = with pkgs; [
      agenix
    ];
  };
}
