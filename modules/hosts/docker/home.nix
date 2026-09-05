{ self, lib, ... }: {
  modules.home.root-docker-home = {
    imports = [
      # self.modules.home.agenix-integration
      self.modules.home.stylix-integration

      self.modules.home.america-new-york-time-zone
      self.modules.home.base-package
      self.modules.home.bash-to-zsh-override
      self.modules.home.common
      self.modules.home.en-us-utf-8-locale
      self.modules.home.root-user

      self.modules.home.bash-program
      self.modules.home.bat-program
      self.modules.home.btop-program
      self.modules.home.delta-program
      self.modules.home.eza-program
      self.modules.home.fastfetch-program
      self.modules.home.fzf-program
      self.modules.home.htop-program
      self.modules.home.nh-program
      self.modules.home.oh-my-posh-program
      self.modules.home.root-git-program
      self.modules.home.ssh-program
      self.modules.home.tmux-program
      self.modules.home.zoxide-program
      self.modules.home.zsh-program
    ];

    nix.settings.sandbox = lib.mkForce false;

    home.stateVersion = "26.05";
  };

  homeConfigurations."${self.lib.users.root.username}@docker" =
    self.lib.mkHomeConfiguration "x86_64-linux" self.modules.home.root-docker-home;
}
