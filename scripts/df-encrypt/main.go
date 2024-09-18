package main

import (
	"github.com/hanselrd/dotfiles/internal/log"
)

func main() {
	log.SetupLogger(log.LevelTrace)

	// df-encrypt =
	//   pkgs.writeShellScriptBin "df-encrypt" ''
	//     if [ -d $1 ]; then
	//       for file in $1/*; do
	//         if echo "$file" | grep -v -q '^.*\.age$'; then
	//           ${pkgs.age}/bin/age -a -R $2 "$file" > "$file.age"
	//         fi
	//       done
	//       ${pkgs.atool}/bin/atool -af $1.tar.gz $1/*.age
	//       ${pkgs.age}/bin/age -a -R $2 $1.tar.gz > $1.tar.gz.age
	//       rm $1/*.age $1.tar.gz
	//     elif [ -f $1 ]; then
	//       echo "f"
	//     fi
	//   '';
}
