{ inputs, lib, ... }:
let
  inherit (inputs) home-manager;
in
{
  lib = {
    mkNixosActivationScript =
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

    mkHomeActivationScript =
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
      home-manager.lib.hm.dag.entryAfter ([ "installPackages" ] ++ deps) ''
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
  };
}
