package homemanager

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/generic"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var (
	now    = time.Now()
	nowymd = timefmt.Format(now, "%y%m%d")
)

var outDir string

var ejectCmd = &cobra.Command{
	Use:   "eject",
	Short: "Eject command",
	Long:  "Eject command",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) (err error) {
		if len(outDir) > len("/nix/store/")+32 {
			err = fmt.Errorf("%s is too long", outDir)
			log.Error().Err(err).Send()
		}
		return
	},
	Run: func(cmd *cobra.Command, args []string) {
		shell.Shell(
			fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", _profile),
		)
		pkg1 := generic.First(
			generic.Must2(
				shell.Shell(
					fmt.Sprintf(
						"nix path-info .#homeConfigurations.%s.activationPackage",
						_profile,
					),
				),
			),
		)
		deps1 := generic.First(
			generic.Must2(shell.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg1))),
		)

		pkg2 := generic.First(
			generic.Must2(
				shell.Shell(
					fmt.Sprintf(
						"readlink -f %s/.nix-profile",
						environment.Environment.User.HomeDirectory,
					),
				),
			),
		)
		deps2 := generic.First(
			generic.Must2(shell.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg2))),
		)

		shell.Shell(
			"sort | uniq > eject.dep",
			shell.WithStdin(strings.Join([]string{deps1, deps2}, "\n")),
		)
		shell.Shell("find /nix/store -depth -print | grep -Ff eject.dep | cpio -ov > eject.cpio")

		if !strings.HasSuffix(outDir, "/") {
			outDir += "/"
		}

		sedSearch := fmt.Sprintf("/nix/store/%s-", strings.Repeat(".", 32))[:len(outDir)]

		shell.Shell(
			fmt.Sprintf(
				"sed -i \"/\\/nix\\/store\\/.\\{32\\}-/s@%s@%s@g\" eject.cpio",
				sedSearch,
				outDir,
			),
		)

		err := os.MkdirAll(outDir, 0o700)
		cobra.CheckErr(err)

		shell.Shell("cpio -idmv < eject.cpio")
		shell.Shell(fmt.Sprintf("chmod -R u+w %s", outDir))

		pkg1New := generic.First(
			generic.Must2(
				shell.Shell(
					fmt.Sprintf("sed \"s@%s@%s@g\"", sedSearch, outDir),
					shell.WithStdin(pkg1),
				),
			),
		)
		shell.Shell(
			fmt.Sprintf(
				"cp -av %s/home-files/. %s/",
				pkg1New,
				environment.Environment.User.HomeDirectory,
			),
		)

		pkg2New := generic.First(
			generic.Must2(
				shell.Shell(
					fmt.Sprintf("sed \"s@%s@%s@g\"", sedSearch, outDir),
					shell.WithStdin(pkg2),
				),
			),
		)
		shell.Shell(
			fmt.Sprintf(
				"ln -snfF %s %s/.nix-profile",
				pkg2New,
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	ejectCmd.Flags().
		StringVar(&outDir, "out-dir", fmt.Sprintf("%s/.nix-hme/%s", environment.Environment.User.HomeDirectory, nowymd), "eject output directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
