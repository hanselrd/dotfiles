{ lib, ... }: {
  lib = rec {
    readCommand =
      name:
      { pkgs }:
      buildEnv: buildCommand: lib.fileContents (pkgs.runCommand name buildEnv buildCommand);

    bannerText =
      {
        pkgs,
        font ? "standard",
        width ? 80,
        justify ? "left",
      }:
      text:
      readCommand "banner-text" { inherit pkgs; } { }
        "${lib.getExe pkgs.figlet} \"${text}\" -f ${font} -w ${builtins.toString width} ${
          if justify == "left" then
            "-l"
          else if justify == "center" then
            "-c"
          else if justify == "right" then
            "-r"
          else
            "-x"
        } > $out";

    rainbowText =
      { pkgs }:
      text:
      readCommand "rainbow-text" {
        inherit pkgs;
      } { } "${lib.getExe pkgs.lolcat} -f ${pkgs.writeText "rainbow-text-file" text} > $out";

    pastelText =
      {
        pkgs,
        fgColor ? "default",
        bgColor ? null,
        bold ? false,
        italic ? false,
        underline ? false,
        noNewline ? true,
        escapeStyle ? null,
      }:
      text:
      readCommand "pastel-text" { inherit pkgs; } { }
        "${lib.getExe' pkgs.coreutils "cat"} ${pkgs.writeText "pastel-text-file" text} | ${lib.getExe pkgs.pastel} -f paint ${
          if noNewline then "-n" else ""
        } \"${fgColor}\" ${
          if bgColor != null then "--on \"${bgColor}\"" else ""
        } ${if bold then "--bold" else ""} ${if italic then "--italic" else ""} ${
          if underline then "--underline" else ""
        } | ${lib.getExe pkgs.gnused} 's/\\x1b/${
          if escapeStyle == "bash" then
            "\\\\e"
          else if escapeStyle == "octal" then
            "\\\\033"
          else if escapeStyle == "hex" then
            "\\\\x1b"
          else if escapeStyle == "unicode" then
            "\\\\u001b"
          else if escapeStyle == "unicode-rust" then
            "\\\\u{1b}"
          else
            "\\x1b"
        }/g' > $out";

    ansiText =
      {
        pkgs,
        style ? "clear",
        escapeStyle ? "direct",
      }:
      text:
      let
        ansiStyle = readCommand "ansi-text-style" {
          inherit pkgs;
        } { } "${lib.getExe pkgs.ansi} ${style} --escape-style=${escapeStyle} > $out";
        ansiReset = readCommand "ansi-text-reset" {
          inherit pkgs;
        } { } "${lib.getExe pkgs.ansi} reset --escape-style=${escapeStyle} > $out";
      in
      lib.concatMapStringsSep "\n" (x: ansiStyle + x + ansiReset) (lib.splitString "\n" text);

    currentTimeUtcPretty =
      { pkgs }:
      readCommand "current-time-utc-pretty" { inherit pkgs; } {
        currentTime = builtins.currentTime;
      } "${lib.getExe' pkgs.coreutils "date"} --utc +\"%a %Y-%m-%dT%H:%M:%SZ\" > $out";

    currentTimePretty =
      { pkgs }:
      tz:
      readCommand "current-time-pretty" { inherit pkgs; } {
        buildInputs = [ pkgs.tzdata ];
        currentTime = builtins.currentTime;
      } "TZ=${tz} ${lib.getExe' pkgs.coreutils "date"} +\"%a %Y-%m-%dT%H:%M:%S%z %Z\" > $out";
  };
}
