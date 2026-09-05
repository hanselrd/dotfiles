{ self, lib, ... }: {
  modules = self.lib.eachModule (
    module:
    self.lib.eachUser (user: {
      user =
        { config, pkgs, ... }:
        let
          homeDirectory =
            if user.username == "root" then
              if pkgs.stdenv.hostPlatform.isDarwin then "/var/root" else "/root"
            else if pkgs.stdenv.hostPlatform.isDarwin then
              "/Users/${user.username}"
            else
              "/home/${user.username}";
        in
        lib.mergeAttrsList [
          (lib.optionalAttrs (module == "nixos") {
            users.users.${user.username} = {
              home = homeDirectory;
              description = user.name;
              isNormalUser = true;
              uid = 1000;
              extraGroups = lib.flatten [
                "wheel"
                (lib.optional config.virtualisation.docker.enable "docker")
                (lib.optional config.networking.networkmanager.enable "networkmanager")
              ];
              initialPassword = "password";
              useDefaultShell = true;
            };
          })
          (lib.optionalAttrs (module == "darwin") {
            nix.settings = {
              trusted-users = [ user.username ];
            };

            users.users.${user.username} = {
              home = homeDirectory;
              description = user.name;
              isHidden = false;
            };

            system.primaryUser = user.username;
          })
          (lib.optionalAttrs (module == "home") {
            home = {
              inherit (user) username;
              inherit homeDirectory;
            };
          })
        ];
    })
  );
}
