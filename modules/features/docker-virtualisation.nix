{ self, lib, ... }: {
  modules = self.lib.eachModule (module: {
    docker-virtualisation =
      { pkgs, ... }:
      lib.mergeAttrsList [
        (lib.optionalAttrs (module == "nixos") {
          virtualisation.docker = {
            enable = true;
            autoPrune = {
              enable = true;
              dates = "weekly";
              flags = [ "--all" ];
            };
          };
        })
        (lib.optionalAttrs (module == "home") {
          home.packages = with pkgs; [
            docker
            docker-compose
          ];
        })
      ];
  });
}
