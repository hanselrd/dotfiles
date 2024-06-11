package homemanager

import (
	"fmt"
	"math/big"
	"os"
	"strings"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/rs/zerolog/log"
	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var (
	now        = time.Now()
	nowymd     = timefmt.Format(now, "%y%m%d")
	nowTodHash = big.NewInt(int64(now.Sub(time.Date(now.Year(), now.Month(), now.Day(), 0, 0, 0, 0, now.Location())).Seconds())).
			Text(36)
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

		pkgs := []string{
			lo.T2(
				lo.Must2(
					shell.Shell(
						fmt.Sprintf(
							"nix path-info .#homeConfigurations.%s.activationPackage",
							_profile,
						),
					),
				),
			).A,
			lo.T2(
				lo.Must2(
					shell.Shell(
						fmt.Sprintf(
							"readlink -f %s/.nix-profile",
							environment.Environment.User.HomeDirectory,
						),
					),
				),
			).A,
		}

		deps := lo.Uniq(lo.FlatMap(pkgs,
			func(p string, _ int) []string {
				return strings.Split(
					lo.T2(
						lo.Must2(
							shell.Shell(
								fmt.Sprintf(
									"nix-store -qR %s",
									p,
								),
							),
						),
					).A, "\n")
			}))

		if !strings.HasSuffix(outDir, "/") {
			outDir += "/"
		}

		sedSearch := fmt.Sprintf("/nix/store/%s-", strings.Repeat(".", 32))[:len(outDir)]

		err := os.MkdirAll(outDir, 0o700)
		cobra.CheckErr(err)

		lop.ForEach(deps, func(d string, i int) {
			log.Debug().Str("dep", d).Msg("archiving")
			cpio := fmt.Sprintf("eject.cpio.%d", i)

			shell.Shell(fmt.Sprintf("find %s | cpio -ov > %s", d, cpio))
			shell.Shell(
				fmt.Sprintf(
					"sed -i \"/\\/nix\\/store\\/.\\{32\\}-/s@%s@%s@g\" %s",
					sedSearch,
					outDir,
					cpio,
				),
			)
			shell.Shell(fmt.Sprintf("(while cpio -idmv; do :; done) < %s", cpio))
		})

		shell.Shell("rm -rf eject.cpio.*")
		shell.Shell(fmt.Sprintf("chmod -R u+w %s", outDir))

		pkgsNew := lo.Map(pkgs, func(p string, _ int) string {
			return lo.T2(
				lo.Must2(
					shell.Shell(
						fmt.Sprintf("sed \"s@%s@%s@g\"", sedSearch, outDir),
						shell.WithStdin(p),
					),
				),
			).A
		})

		shell.Shell(
			fmt.Sprintf(
				"cp -av %s/home-files/. %s/",
				pkgsNew[0],
				environment.Environment.User.HomeDirectory,
			),
		)
		shell.Shell(
			fmt.Sprintf(
				"ln -snfF %s %s/.nix-profile",
				pkgsNew[1],
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	ejectCmd.Flags().
		StringVar(&outDir, "out-dir", fmt.Sprintf("%s/.nix/e/%s/%s", environment.Environment.User.HomeDirectory, nowymd, nowTodHash), "eject output directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
