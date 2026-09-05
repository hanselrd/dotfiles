{ self, lib, ... }: {
  modules = self.lib.eachModule (module: {
    zsh-program =
      { config, pkgs, ... }:
      lib.mergeAttrsList [
        {
          programs.zsh = lib.mergeAttrsList [
            { enable = true; }
            (lib.optionalAttrs (module == "home") {
              dotDir = config.home.homeDirectory;
              defaultKeymap = "emacs";
              autosuggestion = {
                enable = true;
              };
              enableCompletion = false;
              syntaxHighlighting = {
                enable = true;
                # styles = {
                #   "main" = "";
                #   "brackets" = "";
                #   "pattern" = "";
                #   "regexp" = "";
                #   "cursor" = "";
                #   "root" = "";
                #   "line" = "";
                # };
              };
              history = {
                expireDuplicatesFirst = true;
              };
              # plugins = with pkgs; [
              #   {
              #     name = "zsh-vi-mode";
              #     src = zsh-vi-mode;
              #     file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
              #   }
              # ];
              profileExtra = ''
                if [ -e ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh ]; then
                  . ${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh
                fi
              '';
            })
          ];
        }
        (lib.optionalAttrs (module == "nixos") { users.defaultUserShell = pkgs.zsh; })
      ];
  });
}
