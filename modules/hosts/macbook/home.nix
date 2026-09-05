{ self, ... }: {
  modules.home.delacruz-macbook-home = {
    imports = [
      self.modules.home.america-new-york-time-zone
      self.modules.home.base-package
      self.modules.home.common
      self.modules.home.delacruz-user
      self.modules.home.en-us-utf-8-locale
    ];

    home.stateVersion = "26.05";
  };

  modules.darwin.delacruz-macbook-home = {
    home-manager.users.${self.lib.users.delacruz.username} = self.modules.home.delacruz-macbook-home;
  };

  homeConfigurations."${self.lib.users.delacruz.username}@macbook" =
    self.lib.mkHomeConfiguration "aarch64-darwin" self.modules.home.delacruz-macbook-home;
}
