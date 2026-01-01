let
  dev_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnGYRLaLdzMnSoMtIfHCoo+OhlSI8TDFn4yaXcpVS5k";
  work0_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHt/ws2NS0UocPs9ajUlXdzp8ndcJs74CynwtJM+ZHxY";
  work1_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFS0MbdOSVFskkxILtRUDTkmmeX1Tz9iCYFm6hB8hXKE";
  work2_user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAKqZkGOPwxJvoh7iSbmQZAVaHmjtzq1x2XBea4UWfNK";

  dev = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH/s0GPuqU8oABuFz6ytahsSVHKWDHFGv1FLTBciBKcJ";

  yellow = [
    dev_user
    work0_user
    work1_user
    work2_user
    dev
  ];

  blue = [
    dev_user
    dev
  ];
in
{
  "modules/home/work/default.nix.age".publicKeys = yellow;
  "modules/home/work/init.sh.age".publicKeys = yellow;
  "bookmarks.html.age".publicKeys = blue;
  "work_email.age".publicKeys = yellow;
}
