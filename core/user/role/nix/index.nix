{
  nixpkgs,
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  nix.registry.nixpkgs.flake = nixpkgs;

  nixpkgs.config.allowUnfree = pkgs.config.allowUnfree;

  nix.package = lib.modules.mkDefault pkgs.nix;

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    sandbox = env.nixSandbox;
    show-trace = true;
  };

  programs.nix-index = lib.core.user.mkProgram "nix-index" {};
}
