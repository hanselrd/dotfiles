{
  config,
  lib,
  pkgs,
  ...
}: let
  df-gen-master-key =
    pkgs.writeShellScriptBin "df-gen-master-key"
    ''
      MASTER_KEY_NAME=master
      ${pkgs.age}/bin/age-keygen -o $1/$MASTER_KEY_NAME.age
      ${pkgs.age}/bin/age-keygen -y -o $1/$MASTER_KEY_NAME.age.pub $1/$MASTER_KEY_NAME.age
    '';

  df-encrypt =
    pkgs.writeShellScriptBin "df-encrypt"
    ''
      if [ -d $1 ]; then
        for file in $1/*; do
          if echo "$file" | grep -v -q '^.*\.age$'; then
            ${pkgs.age}/bin/age -a -R $2 "$file" > "$file.age"
          fi
        done
        ${pkgs.atool}/bin/atool -af $1.tar.gz $1/*.age
        ${pkgs.age}/bin/age -a -R $2 $1.tar.gz > $1.tar.gz.age
        rm $1/*.age $1.tar.gz
      elif [ -f $1 ]; then
        echo "f"
      fi
    '';

  df-decrypt =
    pkgs.writeShellScriptBin "df-decrypt"
    ''
      if echo $1 | grep -q '^.*\.tar.gz.age$'; then
        archive=$(echo $1 | sed "s/\.tar\.gz\.age/\.tar\.gz/g")
        ${pkgs.age}/bin/age -d -i $2 -o $archive $1
        ${pkgs.atool}/bin/atool -xf $archive
        directory=$(echo $archive | sed "s/\.tar\.gz//g")
        for file in $directory/*; do
          if echo "$file" | grep -q '^.*\.age$'; then
            decrypted=$(echo $file | sed "s/\.age//g")
            ${pkgs.age}/bin/age -d -i $2 -o $decrypted $file
          fi
        done
        rm $archive $directory/*.age
      elif echo $1 | grep -q '^.*\.age$'; then
        echo "f"
      fi
    '';
in {
  home.packages = with pkgs; [
    df-gen-master-key
    df-encrypt
    df-decrypt
  ];
}
