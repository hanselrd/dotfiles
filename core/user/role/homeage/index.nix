{
  config,
  lib,
  pkgs,
  ...
}: {
  # homeage = {
  #   identityPaths = [
  #     # "${config.home.homeDirectory}/.ssh/id_ed25519"
  #     # "${config.home.homeDirectory}/.ssh/id_rsa"
  #     "${config.home.homeDirectory}/.keys/2.age"
  #   ];
  #   installationType = "activation";
  #   file."rts" = {
  #     source = ./secrets/rts.sh.age;
  #     copies = ["${config.home.homeDirectory}/.secrets/rts.sh"];
  #   };
  # };
}
