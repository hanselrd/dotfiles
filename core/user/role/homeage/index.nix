{
  config,
  lib,
  pkgs,
  env,
  preset,
  ...
}: {
  homeage =
    lib.modules.mkIf env.roles.homeage.decrypt
    (lib.modules.mkMerge [
      (lib.modules.mkIf
        (preset.system != "nixos" && preset.system != "linux-systemd") {
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
          source = ./secrets/rts.sh.age;
          copies = ["${config.home.homeDirectory}/.secrets/rts.sh"];
        };
      }
    ]);
}
