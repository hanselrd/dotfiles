{
  inputs,
  lib,
  pkgs,
  env,
  ...
}:
let
  inherit (inputs) gitignore;
in
rec {
  currentTimeUtcPretty = builtins.replaceStrings [ "\n" ] [ "" ] (
    builtins.readFile (
      pkgs.runCommand "current-time-utc-pretty" {
        currentTime = builtins.currentTime;
      } "${lib.getExe' pkgs.coreutils "date"} --utc \"+%Y-%m-%dT%H:%M:%SZ\" > $out"
    )
  );

  currentTimePretty =
    tz:
    builtins.replaceStrings [ "\n" ] [ "" ] (
      builtins.readFile (
        pkgs.runCommand "current-time-pretty" {
          buildInputs = [ pkgs.tzdata ];
          currentTime = builtins.currentTime;
        } "TZ=${tz} ${lib.getExe' pkgs.coreutils "date"} \"+%Y-%m-%dT%H:%M:%S%z %Z\" > $out"
      )
    );

  runExternalOnceHome =
    name: script:
    lib.hm.dag.entryAfter [ "installPackages" ] ''
      file=${env.user.cacheDirectory}/nix/activation/${name}
      new_file=${pkgs.writeText "${name}.sh" script}
      if ! ${lib.getExe' pkgs.diffutils "cmp"} -s "$file" "$new_file"; then
        run ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
        run ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
        ${script}
      fi
    '';

  runExternalAlwaysHome =
    name: script:
    lib.hm.dag.entryAfter [ "installPackages" ] ''
      file=${env.user.cacheDirectory}/nix/activation/${name}
      new_file=${pkgs.writeText "${name}.sh" script}
      run ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
      run ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
      ${script}
    '';

  runExternalOnceSystem = name: script: {
    text = ''
      file=/root/.cache/nix/activation/${name}"
      new_file=${pkgs.writeText "${name}.sh" script}
      if ! ${lib.getExe' pkgs.diffutils "cmp"} -s "$file" "$new_file"; then
        ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
        ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
        ${script}
      fi
    '';
  };

  runExternalAlwaysSystem = name: script: {
    text = ''
      file=/root/.cache/nix/activation/${name}"
      new_file=${pkgs.writeText "${name}.sh" script}
      ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
      ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
      ${script}
    '';
  };

  buildGoBin =
    name:
    pkgs.buildGoModule {
      name = "dotfiles-go-bin-${name}";
      src = gitignore.lib.gitignoreSource ../.;
      vendorHash = "sha256-AEmWuHqOu+KCv89/3oX7oRJhUyzrIyyWHbj9Z7eGgbQ=";
      subPackages = [
        "cmd/${name}"
      ];
      env.CGO_ENABLED = 0;
      meta.mainProgram = name;
    };
}
