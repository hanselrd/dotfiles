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
    ../../core/user/role/development/common/index.nix
  ];

  home.packages = with pkgs; [
    age
    ansible
  ];

  programs.bat = lib.core.user.mkProgram "bat" {};

  programs.exa = lib.core.user.mkProgram "exa" {};

  programs.git = lib.core.user.mkProgram "git" {};

  programs.ssh = lib.core.user.mkProgram "ssh" {};

  programs.tmux = lib.core.user.mkProgram "tmux" {};
}
