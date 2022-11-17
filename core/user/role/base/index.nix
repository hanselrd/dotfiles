{
  config,
  lib,
  pkgs,
  ...
}: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = pkgs.config.home.username;
  home.homeDirectory = pkgs.config.home.homeDirectory;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager = lib.core.user.mkProgram "home-manager" {};

  # services.home-manager.autoUpgrade = lib.core.user.mkService "home-manager.autoUpgrade" {};
}
