{
  config,
  lib,
  pkgs,
  env,
  ...
}: let
  cfg = config.roles.user.git;
in {
  options = {
    roles.user.git = {
      enable = lib.mkEnableOption "roles.user.git";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = env.user.name;
      userEmail = env.user.email;
      delta = {
        enable = true;
        options = {
          syntax-theme = "nix-${config.colorScheme.slug}";
          line-numbers = true;
          plus-style = "green bold ul";
          minus-style = "red bold ul";
        };
      };
      difftastic.enable = lib.mkForce false;
      aliases = {
        smart-clone = "!sh ${
          pkgs.writeShellScript "git-smart-clone.sh" ''
            set -e

            repository=$1
            basename=''${repository##*/}
            directory=''${2:-''${basename%.*}}

            ${lib.getExe' pkgs.coreutils "mkdir"} -p $directory
            ${lib.getExe' pkgs.git "git"} clone --bare --filter=blob:none $repository $directory/.bare
            ${lib.getExe' pkgs.coreutils "echo"} "gitdir: ./.bare" > $directory/.git
            ${lib.getExe' pkgs.git "git"} -C $directory config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
            # ${lib.getExe' pkgs.git "git"} -C $directory fetch origin
          ''
        }";
        lgb = "log --graph --pretty=format:'%C(red bold)%h%Creset -%C(yellow bold)%d%Creset %s %C(green bold)(%cr) %C(blue bold)<%an>%Creset%n' --abbrev-commit --date=relative --branches";
        l = "log --graph --oneline --decorate";
        ll = "log --graph --oneline --decorate --branches --tags";
        lll = "log --graph --oneline --decorate --all";
        mnt = "!git fetch origin --no-auto-gc --prune --refetch && git gc --aggressive --prune=now && git fsck";
      };
      extraConfig = {
        # core = {fsmonitor = true;};
        feature = {manyFiles = true;};
        grep = {lineNumber = true;};
        merge = {conflictStyle = "diff3";};
        safe = {directory = "*";};
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
      };
    };

    home.packages = with pkgs; [
      git-crypt
    ];
  };
}
