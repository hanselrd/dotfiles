{
  lib,
  pkgs,
  env,
  ...
}: {
  currentTimeUtcPretty = builtins.replaceStrings ["\n"] [""] (
    builtins.readFile (
      pkgs.runCommand "current-time-utc-pretty" {
        currentTime = builtins.currentTime;
      } "date --utc \"+%Y-%m-%dT%H:%M:%SZ\" > $out"
    )
  );

  currentTimePretty = builtins.replaceStrings ["\n"] [""] (
    builtins.readFile (
      pkgs.runCommand "current-time-pretty" {
        currentTime = builtins.currentTime;
      } "date \"+%Y-%m-%dT%H:%M:%S%z %Z\" > $out"
    )
  );
}