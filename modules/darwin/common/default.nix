{
  inputs,
  config,
  lib,
  pkgs,
  sharedModulesPath,
  env,
  ...
}:
{
  imports = with inputs; [
    agenix.darwinModules.default
    stylix.darwinModules.stylix
    (sharedModulesPath + "/common")
    ../home-manager-integration
  ];

  networking.hostName = env.hostName;
  networking.computerName = env.hostName;
  system.defaults.smb.NetBIOSName = env.hostName;

  nix.nixPath = [ "/etc/nix/path" ];
  environment.etc = lib.mapAttrs' (name: value: {
    name = "nix/path/${name}";
    value.source = value.flake;
  }) config.nix.registry;

  nix.settings = {
    trusted-users = [ env.username ];
  };

  nix.optimise = {
    automatic = true;
  };

  users.users.${env.username} = {
    home = env.homeDirectory;
    description = env.name;
    shell = pkgs.zsh;
  };

  system.primaryUser = env.username;

  programs.zsh.enable = true;

  environment.shells = with pkgs; [ zsh ];

  security.pam.services.sudo_local.touchIdAuth = true;

  security.sudo.extraConfig = ''
    Defaults pwfeedback
    Defaults insults
  '';

  time.timeZone = env.timeZone;

  system.defaults.menuExtraClock.Show24Hour = true;

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
    };
    taps = [ "homebrew/services" ];
    brews = [ ];
    casks = [ ];
  };

  system.configurationRevision = inputs.self.shortRev or "<dirty>";
}
