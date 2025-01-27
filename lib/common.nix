{
  inputs,
  lib,
  pkgs,
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

  runExternalOnce = script: {
    text = script;
    onChange = script;
  };

  runExternalAlways =
    script:
    runExternalOnce ''
      # ${currentTimeUtcPretty}
      ${script}
    '';

  buildGoBin =
    name:
    pkgs.buildGoModule {
      name = "dotfiles-go-bin-${name}";
      src = gitignore.lib.gitignoreSource ../.;
      vendorHash = "sha256-ShGQPXuZvQf0jiHKlwC2J0mC4JSBeHvT6z0YsIj78hk=";
      subPackages = [
        "cmd/${name}"
      ];
      env.CGO_ENABLED = 0;
      meta.mainProgram = name;
    };
}
