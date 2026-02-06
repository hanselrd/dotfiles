{

  inputs,
  lib,
  rootPath,
  ...
}:
let
  inherit (inputs) self;
in
rec {
  decryptSecretModule =
    identity: secretModule: lib.x.decryptSecret identity (secretModule + "/default.nix.age");

  readSecret =
    identity: secret: lib.removeSuffix "\n" (lib.readFile (lib.x.decryptSecret identity secret));

  readCommand =
    name:
    { pkgs }:
    buildEnv: buildCommand:
    lib.removeSuffix "\n" (lib.readFile (pkgs.runCommand name buildEnv buildCommand));

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
        else if "right" then
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
    } "${lib.getExe' pkgs.coreutils "date"} --utc +\"%Y-%m-%dT%H:%M:%SZ\" > $out";

  currentTimePretty =
    { pkgs }:
    tz:
    readCommand "current-time-pretty" { inherit pkgs; } {
      buildInputs = [ pkgs.tzdata ];
      currentTime = builtins.currentTime;
    } "TZ=${tz} ${lib.getExe' pkgs.coreutils "date"} +\"%Y-%m-%dT%H:%M:%S%z %Z\" > $out";

  runExternalHome =
    name:
    {
      config,
      pkgs,
      text,
      runAlways ? false,
      useSymlink ? true,
      ignoreError ? false,
      deps ? [ ],
    }:
    lib.hm.dag.entryAfter ([ "installPackages" ] ++ deps) ''
      file=${config.xdg.cacheHome}/nix/activation/${name}
      new_file=${pkgs.writeShellScript "${name}.sh" text}
      ${
        if !runAlways then
          ''
            if ! ${lib.getExe' pkgs.diffutils "cmp"} -s "$file" "$new_file"; then
          ''
        else
          ""
      }
      ${
        if useSymlink then
          ''
            run ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
            run ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
          ''
        else
          ''
            run ${lib.getExe' pkgs.coreutils "install"} -DT -m 400 "$new_file" "$file"
          ''
      }
      run ${lib.getExe pkgs.dash} "$new_file" ${
        if ignoreError then "|| ${lib.getExe' pkgs.coreutils "true"}" else ""
      }
      ${
        if !runAlways then
          ''
            fi
          ''
        else
          ""
      }
    '';

  runExternalSystem =
    name:
    {
      pkgs,
      text,
      runAlways ? false,
      useSymlink ? true,
      ignoreError ? false,
      deps ? [ ],
    }:
    {
      text = ''
        file=/root/.cache/nix/activation/${name}
        new_file=${pkgs.writeShellScript "${name}.sh" text}
        ${
          if !runAlways then
            ''
              if ! ${lib.getExe' pkgs.diffutils "cmp"} -s "$file" "$new_file"; then
            ''
          else
            ""
        }
        ${
          if useSymlink then
            ''
              ${lib.getExe' pkgs.coreutils "mkdir"} -p $(${lib.getExe' pkgs.coreutils "dirname"} "$file")
              ${lib.getExe' pkgs.coreutils "ln"} -sf "$new_file" "$file"
            ''
          else
            ''
              ${lib.getExe' pkgs.coreutils "install"} -DT -m 400 "$new_file" "$file"
            ''
        }
        ${lib.getExe pkgs.dash} "$new_file" ${
          if ignoreError then "|| ${lib.getExe' pkgs.coreutils "true"}" else ""
        }
        ${
          if !runAlways then
            ''
              fi
            ''
          else
            ""
        }
      '';
      inherit deps;
    };

  winGetInstallExternal =
    pkg:
    {
      deps ? [ "winget0" ],
    }:
    {
      text = ''
        winget.exe install -e --id --disable-interactivity ${pkg}
      '';
      inherit deps;
    };

  buildGoBin =
    name:
    { pkgs }:
    pkgs.buildGoModule {
      name = "dotfiles-go-bin-${name}";
      src = rootPath;
      vendorHash = "sha256-WITe6pibpu96Ip1OM2vM409CJG5eKGUoUme1uFXVPZg=";
      subPackages = [ "cmd/${name}" ];
      goSum = rootPath + "/go.sum";
      ldflags = [
        "-s -w -linkmode=external"
        "-X 'github.com/hanselrd/dotfiles/internal/build.Version=${lib.version}'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.PureEvalMode=${builtins.toString lib.inPureEvalMode}'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.RootDir=${builtins.toString rootPath}'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.Dirty=${builtins.toString (!(self ? shortRev))}'"
      ];
      meta.mainProgram = name;
    };

  importTOON =
    { pkgs }:
    path:
    lib.importJSON (
      pkgs.runCommand "import-toon" { }
        "${lib.getExe' pkgs.nodejs "npx"} @toon-format/cli -d ${path} -o $out"
    );
}
