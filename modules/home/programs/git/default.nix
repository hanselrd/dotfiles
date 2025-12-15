{
  config,
  lib,
  pkgs,
  env,
  ...
}:
{
  programs.git = {
    enable = true;
    settings = {
      user.name = env.name;
      user.email = env.email;
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
          ${lib.getExe pkgs.git} clone --bare --filter=blob:none "$repository" "$directory/.bare"
          ${lib.getExe' pkgs.coreutils "echo"} "gitdir: ./.bare" > "$directory/.git"
          ${lib.getExe pkgs.git} -C "$directory" config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
          # ${lib.getExe pkgs.git} -C "$directory" fetch origin
        ''}";
        sworktree = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree.sh" ''
          set -e

          branch=$1
          directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")

          ${lib.getExe pkgs.git} worktree add "$directory" "$branch"
        ''}";
        sworktree-new = "!${lib.getExe pkgs.dash} ${pkgs.writeShellScript "git-smart-worktree-new.sh" ''
          set -e

          branch=$1
          directory=$(${lib.getExe' pkgs.coreutils "echo"} "$branch" | ${lib.getExe pkgs.gnused} "s@/@-@g")
          parent=$2

          ${lib.getExe pkgs.git} worktree add -b "$branch" "$directory" "$parent"
        ''}";

        audit = "!${lib.getExe pkgs.git} count-objects --verbose --human-readable";
        hydrate = "!${lib.getExe pkgs.git} fetch origin --no-auto-gc --prune --refetch";
        mnt = "!${lib.getExe pkgs.git} hydrate && ${lib.getExe pkgs.git} gc --aggressive --prune=now && ${lib.getExe pkgs.git} fsck";

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
    git-filter-repo
  ];
}
