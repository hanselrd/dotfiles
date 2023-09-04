{
  config,
  lib,
  pkgs,
  env,
  ...
}: {
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

          mkdir -p $directory
          git clone --bare --filter=blob:none $repository $directory/.bare
          echo "gitdir: ./.bare" > $directory/.git
          git -C $directory config remote.origin.fetch "+refs/heads/*:refs/remotes/origin/*"
          # git -C $directory fetch origin
        ''
      }";
    };
  };
}
