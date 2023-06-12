{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
  nixpkgs.config.allowUnfree = pkgs.config.allowUnfree;

  xdg.configFile = {
    "nix/nix.conf".text = ''
      experimental-features = nix-command flakes
      sandbox = ${lib.trivial.boolToString env.nixSandbox}
    '';
  };

  programs.nix-index = lib.core.user.mkProgram "nix-index" {};
}
