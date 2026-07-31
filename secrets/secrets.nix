let
  dev_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnGYRLaLdzMnSoMtIfHCoo+OhlSI8TDFn4yaXcpVS5k";
  work0_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHt/ws2NS0UocPs9ajUlXdzp8ndcJs74CynwtJM+ZHxY";
  work1_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFS0MbdOSVFskkxILtRUDTkmmeX1Tz9iCYFm6hB8hXKE";
  work2_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAKqZkGOPwxJvoh7iSbmQZAVaHmjtzq1x2XBea4UWfNK";

  dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH/s0GPuqU8oABuFz6ytahsSVHKWDHFGv1FLTBciBKcJ";

  matrix = [
    {
      secrets = [
        "modules/home/work/default.nix"
        "modules/home/work/init.sh"
        "modules/home/work/init2.sh"
        "modules/home/work/rts.hs"
        "src/Dotfiles/Secrets/Scripts.hs"
        "work-email"
      ];
      keys = [
        dev_user
        work0_user
        work1_user
        work2_user
        dev
      ];
    }
    {
      secrets = [
        "bookmarks.html"
        "smb-data-creds"
      ];
      keys = [
        dev_user
        dev
      ];
    }
  ];
in
builtins.foldl' (
  acc: attrs:
  acc
  // (builtins.listToAttrs (
    builtins.map (secret: {
      name = "${secret}.age";
      value = {
        publicKeys = attrs.keys;
        armor = true;
      };
    }) attrs.secrets
  ))
) { } matrix
