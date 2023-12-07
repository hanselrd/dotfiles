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
          syntax-theme = "nix-${pkgs.config.colorScheme.slug}";
          line-numbers = true;
          plus-style = "green bold ul";
          minus-style = "red bold ul";
        };
      };
      extraConfig = {
        safe = {directory = "*";};
        grep = {lineNumber = true;};
        merge = {conflictStyle = "diff3";};
        advice = {detachedHead = false;};
        color = {ui = "auto";};
        color."branch" = {
          current = "yellow reverse";
          local = "yellow";
          remote = "green";
        };
        color."diff" = {
          meta = "yellow bold";
          frag = "magenta bold";
          old = "red bold";
          new = "green bold";
        };
        color."status" = {
          added = "yellow";
          changed = "green";
          untracked = "cyan";
        };
        alias = {
          smart-clone = "!sh ${
            pkgs.writeShellScript "git-smart-clone.sh"
            ''
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
        };
      };
    };

    home.packages = with pkgs; [
      git-crypt
    ];
  };
}
