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
              sclone = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-clone.sh" ''
                set -e

                repository=$1
                basename=''${repository##*/}
                directory=''${2:-''${basename%.*}}

                ${lib.getExe' pkgs.coreutils "mkdir"} -p "$directory"
                ${lib.getExe pkgs.git} clone --bare --single-branch --filter=blob:none "$repository" "$directory/.bare"
                ${lib.getExe' pkgs.coreutils "echo"} "gitdir: ./.bare" > "''${directory}/.git"
                # ${lib.getExe pkgs.git} -C "$directory" config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
                ${lib.getExe pkgs.git} -C "$directory" config --unset-all remote.origin.fetch
                # ${lib.getExe pkgs.git} -C "$directory" fetch origin
              ''}";
              swt-add = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree-add.sh" ''
                set -e

                branch=$1
                directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")

                ${lib.getExe pkgs.git} fetch origin "''${branch}:refs/remotes/origin/''${branch}"
                ${lib.getExe pkgs.git} branch "$branch" "origin/''${branch}" --track 2>/dev/null || true
                ${lib.getExe pkgs.git} remote set-branches --add origin "$branch"
                ${lib.getExe pkgs.git} worktree add "$directory" "$branch"
              ''}";
              swt-new = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree-new.sh" ''
                set -e

                branch=$1
                directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")
                parent=$2

                ${lib.getExe pkgs.git} fetch origin "''${parent}:refs/remotes/origin/''${branch}" 2>/dev/null || true

                if ${lib.getExe pkgs.git} rev-parse --verify "origin/''${parent}" >/dev/null 2>&1; then
                  ${lib.getExe pkgs.git} branch "$branch" "origin/''${parent}"
                else
                  ${lib.getExe pkgs.git} branch "$branch" "$parent"
                fi

                ${lib.getExe pkgs.git} remote set-branches --add origin "$branch"
                ${lib.getExe pkgs.git} worktree add "$directory" "$branch"

                ${lib.getExe pkgs.git} -C "$directory" branch --set-upstream-to="origin/''${branch}" "$branch" 2>/dev/null || true
              ''}";
              swt-rm = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree-rm.sh" ''
                set -e

                branch=$1
                directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")

                ${lib.getExe' pkgs.coreutils "rm"} -rf "$directory"
                ${lib.getExe pkgs.git} worktree prune
                ${lib.getExe pkgs.git} branch -D "$branch" 2>/dev/null || true
                ${lib.getExe pkgs.git} config --unset remote.origin.fetch "refs/heads/''${branch}:refs/remotes/origin/''${branch}" 2>/dev/null || true
                ${lib.getExe pkgs.git} config --unset remote.origin.fetch "+refs/heads/''${branch}:refs/remotes/origin/''${branch}" 2>/dev/null || true
                ${lib.getExe pkgs.git} repack -d -l
                ${lib.getExe pkgs.git} prune --expire=now
              ''}";

              audit = "!${git-alias} audit";
              hydrate = "!${git-alias} hydrate";
              mnt = "!${git-alias} maintenance";

              lgb = "!${lib.getExe pkgs.git} log --graph --pretty=format:'%C(red bold)%h%Creset -%C(yellow bold)%d%Creset %s %C(green bold)(%cr) %C(blue bold)<%an>%Creset%n' --abbrev-commit --date=relative --branches";
              l = "!${lib.getExe pkgs.git} log --graph --oneline --decorate";
              ll = "!${lib.getExe pkgs.git} log --graph --oneline --decorate --branches --tags";
              lll = "!${lib.getExe pkgs.git} log --graph --oneline --decorate --all";
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
