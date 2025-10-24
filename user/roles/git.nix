{
  config,
  lib,
  pkgs,
  env,
  ...
}:
let
  cfg = config.roles.user.git;
in
{
  options = {
    roles.user.git = {
      enable = lib.mkEnableOption "roles.user.git";
    };
  };

  config = lib.mkIf cfg.enable {
    roles.user.delta.enable = true;

    programs.git = {
      enable = true;
      settings = {
        user.name = env.user.name;
        user.email = env.user.email;
        # core = {
        #   fsmonitor = true;
        # };
        feature = {
          manyFiles = true;
        };
        grep = {
          lineNumber = true;
        };
        init = {
          defaultBranch = lib.mkForce "master";
        };
        merge = {
          conflictStyle = "diff3";
        };
        safe = {
          directory = "*";
        };
        # color = {
        #   branch = {
        #     current = "yellow bold reverse";
        #     local = "yellow bold";
        #     remote = "green bold";
        #     upstream = "magenta bold";
        #   };
        #   diff = {
        #     meta = "yellow bold";
        #     frag = "magenta bold";
        #     old = "red bold";
        #     new = "green bold";
        #   };
        #   status = {
        #     added = "yellow bold";
        #     changed = "green bold";
        #     untracked = "cyan bold";
        #   };
        # };
        alias = {
          sclone = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-clone.sh" ''
            set -e

            repository=$1
            basename=''${repository##*/}
            directory=''${2:-''${basename%.*}}

            ${lib.getExe' pkgs.coreutils "mkdir"} -p "$directory"
            ${lib.getExe' pkgs.git "git"} clone --bare --filter=blob:none "$repository" "$directory/.bare"
            ${lib.getExe' pkgs.coreutils "echo"} "gitdir: ./.bare" > "$directory/.git"
            ${lib.getExe' pkgs.git "git"} -C "$directory" config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
            # ${lib.getExe' pkgs.git "git"} -C "$directory" fetch origin
          ''}";
          sworktree = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree.sh" ''
            set -e

            branch=$1
            directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")

            ${lib.getExe' pkgs.git "git"} worktree add "$directory" "$branch"
          ''}";
          sworktree-new = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree-new.sh" ''
            set -e

            branch=$1
            directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")
            parent=$2

            ${lib.getExe' pkgs.git "git"} worktree add -b "$branch" "$directory" "$parent"
          ''}";

          audit = "!${lib.getExe' pkgs.git "git"} count-objects --verbose --human-readable";
          hydrate = "!${lib.getExe' pkgs.git "git"} fetch origin --no-auto-gc --prune --refetch";
          mnt = "!${lib.getExe' pkgs.git "git"} hydrate && ${lib.getExe' pkgs.git "git"} gc --aggressive --prune=now && ${lib.getExe' pkgs.git "git"} fsck";

          lgb = "log --graph --pretty=format:'%C(red bold)%h%Creset -%C(yellow bold)%d%Creset %s %C(green bold)(%cr) %C(blue bold)<%an>%Creset%n' --abbrev-commit --date=relative --branches";
          l = "log --graph --oneline --decorate";
          ll = "log --graph --oneline --decorate --branches --tags";
          lll = "log --graph --oneline --decorate --all";
        };
      };
    };

    home.packages = with pkgs; [
      # git-repair
      git-crypt
      git-filter-repo
    ];
  };
}
