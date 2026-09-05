{ self, ... }: {
  modules.nixos = self.lib.eachUser (user: {
    motd =
      { config, pkgs, ... }:
      let
        inherit (config)
          boot
          networking
          system
          time
          ;
      in
      {
        users.motd = with config.lib.stylix.colors.withHashtag; ''
          ${self.lib.pastelText
            {
              inherit pkgs;
              fgColor = bright-blue;
              bold = true;
            }
            (
              self.lib.bannerText {
                inherit pkgs;
                font = "small";
              } "hanselrd"
            )
          }
          ${self.lib.pastelText
            {
              inherit pkgs;
              fgColor = "gray";
              bold = true;
            }
            (
              self.lib.bannerText {
                inherit pkgs;
                font = "mini";
              } networking.fqdnOrHostName
            )
          }

          ${self.lib.pastelText
            {
              inherit pkgs;
              fgColor = bright-red;
              bold = true;
            }
            ''
              UNAUTHORIZED ACCESS TO THIS DEVICE IS PROHIBITED

              You must have explicit, authorized permission to access or configure this
              device. Unauthorized attempts and actions to access or use this system may
              result in civil and/or criminal penalties. All activities performed on this
              device are logged and monitored.
            ''
          }

          ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = "gray";
              bold = true;
            } "${networking.fqdnOrHostName}"
          }: rev: ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-green;
              bold = true;
            } system.configurationRevision
          } @ ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-magenta;
              bold = true;
            } (self.lib.currentTimePretty { inherit pkgs; } time.timeZone)
          } by ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-cyan;
              bold = true;
            } user.username
          }

          host:    ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = "gray";
              bold = true;
            } networking.fqdnOrHostName
          }
          os:      ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-blue;
              bold = true;
            } "NixOS ${system.nixos.release} (${system.nixos.codeName})"
          }
          version: ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-magenta;
              bold = true;
            } system.nixos.version
          }
          kernel:  ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = "gray";
              bold = true;
            } boot.kernelPackages.kernel.version
          }
          nix:     ${
            self.lib.pastelText {
              inherit pkgs;
              fgColor = bright-blue;
              bold = true;
            } pkgs.nix.version
          }
        '';
      };
  });
}
