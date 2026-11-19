let
  dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH/s0GPuqU8oABuFz6ytahsSVHKWDHFGv1FLTBciBKcJ";

  devUser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnGYRLaLdzMnSoMtIfHCoo+OhlSI8TDFn4yaXcpVS5k";
  work0User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHt/ws2NS0UocPs9ajUlXdzp8ndcJs74CynwtJM+ZHxY";
  work1User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFS0MbdOSVFskkxILtRUDTkmmeX1Tz9iCYFm6hB8hXKE";
  work2User = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAKqZkGOPwxJvoh7iSbmQZAVaHmjtzq1x2XBea4UWfNK";

  matrix = [
    {
      secrets = [
        "modules/features/work/default.nix"
        "modules/features/work/init.sh"
        "modules/features/work/init2.sh"
        "src/bin/dune"
        "src/bin/rts.ml"
        "src/lib/dune"
        "src/lib/scripts.ml"
        "work-email"
      ];
      keys = [
        dev
        devUser
        work0User
        work1User
        work2User
      ];
    }
    {
      secrets = [
        "bookmarks.html"
        "claude-code-oauth-token"
        "smb-data-creds"
      ];
      keys = [
        dev
        devUser
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
