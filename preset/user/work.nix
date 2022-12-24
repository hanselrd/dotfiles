{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../core/user/role/common/index.nix
    ../../core/user/role/nix/index.nix
    ../../core/user/role/shell/index.nix
    ../../core/user/role/editor/index.nix
    ../../core/user/role/pager/index.nix
    ../../core/user/role/other/index.nix
  ];

  home.packages = with pkgs; [
    age
    ansible
  ];
}
