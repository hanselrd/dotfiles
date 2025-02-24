{
  inputs,
  lib,
  pkgs,
  env,
  ...
}:
let
  inherit (inputs) self gitignore;
in
{
  currentTimeUtcPretty = lib.replaceStrings [ "\n" ] [ "" ] (
    lib.readFile (
      pkgs.runCommand "current-time-utc-pretty" {
        currentTime = builtins.currentTime;
      } "${lib.getExe' pkgs.coreutils "date"} --utc \"+%Y-%m-%dT%H:%M:%SZ\" > $out"
    )
  );

  currentTimePretty =
    tz:
    lib.replaceStrings [ "\n" ] [ "" ] (
      lib.readFile (
        pkgs.runCommand "current-time-pretty" {
          buildInputs = [ pkgs.tzdata ];
          currentTime = builtins.currentTime;
        } "TZ=${tz} ${lib.getExe' pkgs.coreutils "date"} \"+%Y-%m-%dT%H:%M:%S%z %Z\" > $out"
      )
    );

  runExternalHome =
    name:
    {
      text,
      runAlways ? false,
      useSymlink ? true,
      ignoreError ? false,
      deps ? [ ],
    }:
    lib.hm.dag.entryAfter ([ "installPackages" ] ++ deps) ''
      file=${env.user.cacheDirectory}/nix/activation/${name}
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
    pkgs.buildGoModule {
      name = "dotfiles-go-bin-${name}";
      src = gitignore.lib.gitignoreSource ../.;
      vendorHash = "sha256-LR98aP6Q8dY2F+2oiN6JkaYKNIwX7mJ2pfifb8JOEbQ=";
      subPackages = [
        "cmd/${name}"
      ];
      ldflags = [
        "-s -w -linkmode=external"
        "-X 'github.com/hanselrd/dotfiles/internal/build.Version=${lib.version}'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.PureEvalMode=${builtins.toString lib.inPureEvalMode}'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.RootDir=${
          if !lib.inPureEvalMode then builtins.getEnv "PWD" else gitignore.lib.gitignoreSource ../.
        }'"
        "-X 'github.com/hanselrd/dotfiles/internal/build.Dirty=${
          builtins.toString (!(lib.hasAttr "shortRev" self))
        }'"
      ];
      meta.mainProgram = name;
    };
}
