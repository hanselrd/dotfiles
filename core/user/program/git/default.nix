{
  config,
  lib,
  pkgs,
  ...
}: {
  userName = config.home.name;
  userEmail = "18725263+hanselrd@users.noreply.github.com";
  delta = {
    enable = true;
    options = {
      line-numbers = true;
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
  };
}
