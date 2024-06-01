package homemanager

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles"
	"github.com/hanselrd/dotfiles/lib/utils"
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
		utils.Shell(
			fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", profile),
		)
		pkg1 := utils.First(
			utils.Must2(
				utils.Shell(
					fmt.Sprintf("nix path-info .#homeConfigurations.%s.activationPackage", profile),
				),
			),
		)
		deps1 := utils.First(
			utils.Must2(utils.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg1))),
		)

		pkg2 := utils.First(
			utils.Must2(
				utils.Shell(
					fmt.Sprintf(
						"readlink -f %s/.nix-profile",
						dotfiles.Environment.User.HomeDirectory,
					),
				),
			),
		)
		deps2 := utils.First(
			utils.Must2(utils.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg2))),
		)

		utils.Shell(
			"sort | uniq > eject.dep",
			utils.WithStdin(strings.Join([]string{deps1, deps2}, "\n")),
		)
		utils.Shell("find /nix/store -depth -print | grep -Ff eject.dep | cpio -ov > eject.cpio")

		if !strings.HasSuffix(outDir, "/") {
			outDir += "/"
		}

		sedSearch := fmt.Sprintf("/nix/store/%s-", strings.Repeat(".", 32))[:len(outDir)]

		utils.Shell(
			fmt.Sprintf(
				"sed -i \"/\\/nix\\/store\\/.\\{32\\}-/s@%s@%s@g\" eject.cpio",
				sedSearch,
				outDir,
			),
		)

		err := os.MkdirAll(outDir, 0700)
		cobra.CheckErr(err)

		utils.Shell("cpio -idmv < eject.cpio")
		utils.Shell(fmt.Sprintf("chmod -R u+w %s", outDir))

		pkg1New := utils.First(
			utils.Must2(
				utils.Shell(
					fmt.Sprintf("sed \"s@%s@%s@g\"", sedSearch, outDir),
					utils.WithStdin(pkg1),
				),
			),
		)
		utils.Shell(
			fmt.Sprintf(
				"cp -av %s/home-files/. %s/",
				pkg1New,
				dotfiles.Environment.User.HomeDirectory,
			),
		)

		pkg2New := utils.First(
			utils.Must2(
				utils.Shell(
					fmt.Sprintf("sed \"s@%s@%s@g\"", sedSearch, outDir),
					utils.WithStdin(pkg2),
				),
			),
		)
		utils.Shell(
			fmt.Sprintf(
				"ln -snfF %s %s/.nix-profile",
				pkg2New,
				dotfiles.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	ejectCmd.Flags().
		StringVar(&outDir, "out-dir", fmt.Sprintf("%s/.nix-hme/%s", dotfiles.Environment.User.HomeDirectory, nowymd), "eject output directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
