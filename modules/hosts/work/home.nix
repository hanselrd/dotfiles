{ self, ... }: {
  modules.home.delacruz-work-work-home = {
    imports = [
      # self.modules.home.agenix-integration
      self.modules.home.stylix-integration

      self.modules.home.america-new-york-time-zone
      self.modules.home.base-package
      self.modules.home.bash-to-zsh-override
      self.modules.home.common
      self.modules.home.delacruz-work-user
      self.modules.home.en-us-utf-8-locale
      self.modules.home.work-secret

      self.modules.home.ccache-program
      self.modules.home.bash-program
      self.modules.home.bat-program
      self.modules.home.btop-program
      self.modules.home.delacruz-work-git-program
      self.modules.home.delacruz-work-neovim-program
      self.modules.home.delta-program
      self.modules.home.eza-program
      self.modules.home.fastfetch-program
      self.modules.home.fzf-program
      self.modules.home.gdb-program
      self.modules.home.home-manager-program
      self.modules.home.htop-program
      self.modules.home.lldb-program
      self.modules.home.nh-program
      self.modules.home.oh-my-posh-program
      self.modules.home.ripgrep-program
      self.modules.home.ssh-program
      self.modules.home.tmux-program
      self.modules.home.zoxide-program
      self.modules.home.zsh-program

      self.modules.home.cpp-development
      self.modules.home.java-development
      self.modules.home.python-development
      self.modules.home.shell-development
    ];

    home.stateVersion = "26.05";
  };

  modules.home.hansel-delacruz-work-work-home = {
    imports = [
      # self.modules.home.agenix-integration
      self.modules.home.stylix-integration

      self.modules.home.america-new-york-time-zone
      self.modules.home.base-package
      self.modules.home.bash-to-zsh-override
      self.modules.home.common
      self.modules.home.en-us-utf-8-locale
      self.modules.home.hansel-delacruz-work-user

      self.modules.home.bash-program
      self.modules.home.bat-program
      self.modules.home.btop-program
      self.modules.home.ccache-program
      self.modules.home.delta-program
      self.modules.home.eza-program
      self.modules.home.fastfetch-program
      self.modules.home.fzf-program
      self.modules.home.gdb-program
      self.modules.home.hansel-delacruz-work-git-program
      self.modules.home.hansel-delacruz-work-neovim-program
      self.modules.home.home-manager-program
      self.modules.home.htop-program
      self.modules.home.lldb-program
      self.modules.home.nh-program
      self.modules.home.oh-my-posh-program
      self.modules.home.ripgrep-program
      self.modules.home.ssh-program
      self.modules.home.tmux-program
      self.modules.home.zoxide-program
      self.modules.home.zsh-program

      self.modules.home.cpp-development
      self.modules.home.java-development
      self.modules.home.python-development
      self.modules.home.shell-development
    ];

    home.stateVersion = "26.05";
  };

  homeConfigurations."${self.lib.users.delacruz-work.username}@work" =
    self.lib.mkHomeConfiguration "x86_64-linux" self.modules.home.delacruz-work-work-home;

  homeConfigurations."${self.lib.users.hansel-delacruz-work.username}@work" =
    self.lib.mkHomeConfiguration "x86_64-linux" self.modules.home.hansel-delacruz-work-work-home;
}
