{
  inputs,
  lib,
  pkgs,
  ...
}: let
  inherit (inputs) gitignore;
in rec {
  currentTimeUtcPretty = builtins.replaceStrings ["\n"] [""] (
    builtins.readFile (
      pkgs.runCommand "current-time-utc-pretty" {
        currentTime = builtins.currentTime;
      } "${lib.getExe' pkgs.coreutils "date"} --utc \"+%Y-%m-%dT%H:%M:%SZ\" > $out"
    )
  );

  currentTimePretty = tz:
    builtins.replaceStrings ["\n"] [""] (
      builtins.readFile (
        pkgs.runCommand "current-time-pretty" {
          buildInputs = [pkgs.tzdata];
          currentTime = builtins.currentTime;
        } "TZ=${tz} ${lib.getExe' pkgs.coreutils "date"} \"+%Y-%m-%dT%H:%M:%S%z %Z\" > $out"
      )
    );

  runExternal = script: {
    text = script;
    onChange = script;
  };

  runExternalAlways = script:
    runExternal ''
      # ${currentTimeUtcPretty}
      ${script}
    '';

  buildGoScript = name:
    pkgs.buildGoModule {
      name = "dotfiles-go-script-${name}";
      src = gitignore.lib.gitignoreSource ../.;
      vendorHash = "sha256-PUVCB+9KHVKlp2uk/0CyWeXjumW5IG7R7w8iZRwfr+A=";
      subPackages = [
        "scripts/${name}"
      ];
      CGO_ENABLED = 0;
      meta.mainProgram = name;
    };
}
