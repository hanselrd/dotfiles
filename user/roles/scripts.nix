{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.roles.user.scripts;
  # writeShellApp = name: runtimeInputs: attrs:
  #   pkgs.writeShellApplication {
  #     inherit name runtimeInputs;
  #     text = let
  #       loop = depth: arg0:
  #         if builtins.isAttrs arg0
  #         then
  #           lib.concatStringsSep "\n" (
  #             [
  #               ''
  #                 if [ "$#" -lt ${builtins.toString depth} ]; then
  #                   echo "Usage ${lib.concatMapStringsSep " " (x: "\$${builtins.toString x}") (builtins.genList (lib.id) depth)} <${lib.concatStringsSep "|" (lib.attrNames arg0)}>"
  #                   exit 1
  #                 fi
  #                 case "''$${builtins.toString depth}" in
  #               ''
  #             ]
  #             ++ (lib.mapAttrsToList (
  #                 name: value: ''
  #                   ${name})
  #                   ${loop (depth + 1) value}
  #                   ;;
  #                 ''
  #               )
  #               arg0)
  #             ++ ["esac"]
  #           )
  #         else arg0;
  #     in
  #       loop 1 attrs;
  #   };
  # df-master-key = writeShellApp "df-master-key" (with pkgs; [age]) {
  #   generate = ''
  #     age-keygen -o "$2/master.age"
  #     age-keygen -y -o "$2/master.age.pub" "$2/master.age"
  #   '';
  # };
  # df-encrypt =
  #   pkgs.writeShellScriptBin "df-encrypt"
  #   ''
  #     if [ -d $1 ]; then
  #       for file in $1/*; do
  #         if echo "$file" | grep -v -q '^.*\.age$'; then
  #           ${pkgs.age}/bin/age -a -R $2 "$file" > "$file.age"
  #         fi
  #       done
  #       ${pkgs.atool}/bin/atool -af $1.tar.gz $1/*.age
  #       ${pkgs.age}/bin/age -a -R $2 $1.tar.gz > $1.tar.gz.age
  #       rm $1/*.age $1.tar.gz
  #     elif [ -f $1 ]; then
  #       echo "f"
  #     fi
  #   '';
  # df-decrypt =
  #   pkgs.writeShellScriptBin "df-decrypt"
  #   ''
  #     if echo $1 | grep -q '^.*\.tar.gz.age$'; then
  #       archive=$(echo $1 | sed "s/\.tar\.gz\.age/\.tar\.gz/g")
  #       ${pkgs.age}/bin/age -d -i $2 -o $archive $1
  #       ${pkgs.atool}/bin/atool -xf $archive
  #       directory=$(echo $archive | sed "s/\.tar\.gz//g")
  #       for file in $directory/*; do
  #         if echo "$file" | grep -q '^.*\.age$'; then
  #           decrypted=$(echo $file | sed "s/\.age//g")
  #           ${pkgs.age}/bin/age -d -i $2 -o $decrypted $file
  #         fi
  #       done
  #       rm $archive $directory/*.age
  #     elif echo $1 | grep -q '^.*\.age$'; then
  #       echo "f"
  #     fi
  #   '';
in {
  options = {
    roles.user.scripts = {
      enable = lib.mkEnableOption "roles.user.scripts";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # df-master-key
      # df-encrypt
      # df-decrypt
      dotfiles-scripts
      # dotfiles-scripts2
    ];
  };
}
