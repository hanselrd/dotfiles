{ self, lib, ... }: {
  modules.darwin.macbook-host = {
    imports = [ ];

    # TODO: source from hardware instead
    nixpkgs.hostPlatform = lib.mkDefault "aarch64-darwin";

    networking.hostName = "macbook";

    system.stateVersion = 7;
  };

  darwinConfigurations.macbook = self.lib.mkDarwinConfiguration self.modules.darwin.macbook-host;
}
