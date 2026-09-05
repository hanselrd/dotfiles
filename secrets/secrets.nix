let
  devUser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnGYRLaLdzMnSoMtIfHCoo+OhlSI8TDFn4yaXcpVS5k";
  work0User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHt/ws2NS0UocPs9ajUlXdzp8ndcJs74CynwtJM+ZHxY";
  work1User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFS0MbdOSVFskkxILtRUDTkmmeX1Tz9iCYFm6hB8hXKE";
  work2User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAKqZkGOPwxJvoh7iSbmQZAVaHmjtzq1x2XBea4UWfNK";

  dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH/s0GPuqU8oABuFz6ytahsSVHKWDHFGv1FLTBciBKcJ";

  matrix = [
    {
      secrets = [
        "modules/features/work/default.nix"
        "modules/features/work/init.sh"
        "modules/features/work/init2.sh"
        "modules/features/work/rts.hs"
        "src/Dotfiles/Secrets/Scripts.hs"
        "work-email"
      ];
      keys = [
        devUser
        work0User
        work1User
        work2User
        dev
      ];
    }
    {
      secrets = [
        "bookmarks.html"
        "smb-data-creds"
      ];
      keys = [
        devUser
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
