package main

import (
	"github.com/hanselrd/dotfiles/internal/log"
)

func main() {
	log.SetupLogger(log.LevelTrace)

	// df-decrypt =
	//   pkgs.writeShellScriptBin "df-decrypt" ''
	//     if echo $1 | grep -q '^.*\.tar.gz.age$'; then
	//       archive=$(echo $1 | sed "s/\.tar\.gz\.age/\.tar\.gz/g")
	//       ${pkgs.age}/bin/age -d -i $2 -o $archive $1
	//       ${pkgs.atool}/bin/atool -xf $archive
	//       directory=$(echo $archive | sed "s/\.tar\.gz//g")
	//       for file in $directory/*; do
	//         if echo "$file" | grep -q '^.*\.age$'; then
	//           decrypted=$(echo $file | sed "s/\.age//g")
	//           ${pkgs.age}/bin/age -d -i $2 -o $decrypted $file
	//         fi
	//       done
	//       rm $archive $directory/*.age
	//     elif echo $1 | grep -q '^.*\.age$'; then
	//       echo "f"
	//     fi
	//   '';
}
