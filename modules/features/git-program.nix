{ self, lib, ... }: {
  modules.home = self.lib.eachUser (user: {
    git-program =
      { pkgs, ... }:
      let
        git-alias = lib.getExe' self.packages.${pkgs.stdenv.system}.dotfiles "git-alias";
      in
      {
        programs.git = {
          enable = true;
          lfs.enable = true;
          settings = {
            user = { inherit (user) name email; };
            log.date = "format:${self.lib.timeFormat}";
            commit.verbose = true;
            diff = {
              algorithm = "histogram";
              colorMoved = "default";
              mnemonicPrefix = true;
            };
            feature.manyFiles = true;
            grep.lineNumber = true;
            init.defaultBranch = lib.mkForce "master";
            merge.conflictStyle = "zdiff3";
            safe.directory = "*";
            filter = {
              secrets = {
                smudge = lib.getExe' pkgs.coreutils "cat";
                clean = "${lib.getExe' pkgs.coreutils "printf"} ''";
                required = true;
              };
            };
            alias = {
              sclone = "!${git-alias} smart-clone";
              swt-add = "!${git-alias} smart-worktree-add";
              swt-rm = "!${git-alias} smart-worktree-remove";

              audit = "!${git-alias} audit";
              hydrate = "!${git-alias} hydrate";
              mnt = "!${git-alias} maintenance";
            };
          };
        };

        home.packages = with pkgs; [
          # git-repair
          git-crypt
          git-extras
          git-filter-repo
        ];
      };
  });
}
