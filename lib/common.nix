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
}
