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
{
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

  buildGoBin =
    name:
    pkgs.buildGoModule {
      name = "dotfiles-go-bin-${name}";
      src = gitignore.lib.gitignoreSource ../.;
      vendorHash = "sha256-Rv7ur/ZrsjcnKq/3LRMSJtr2YHKuuYAGzLswSUGuFEQ=";
      subPackages = [
        "cmd/${name}"
      ];
      env.CGO_ENABLED = 0;
      meta.mainProgram = name;
    };
}
