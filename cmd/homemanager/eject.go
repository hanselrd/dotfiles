package homemanager

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/rs/zerolog/log"
	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/hash"
	"github.com/hanselrd/dotfiles/internal/nix"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/internal/shellx"
	"github.com/hanselrd/dotfiles/pkg/environment"
)

var (
	now    = time.Now()
	outDir string
)

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
		nix.BuildHomeManagerConfiguration(profileGroup)

		paths := nix.FindHomeManagerEjectPaths(profileGroup)
		deps := nix.FindHomeManagerEjectDependencies(profileGroup)

		if !strings.HasSuffix(outDir, "/") {
			outDir += "/"
		}

		sedSearch := fmt.Sprintf("/nix/store/%s-", strings.Repeat(".", 32))[:len(outDir)]

		err := os.MkdirAll(outDir, 0o700)
		cobra.CheckErr(err)

		lop.ForEach(deps, func(d string, i int) {
			log.Debug().Str("dep", d).Msg("archiving")
			cpio := fmt.Sprintf("eject.cpio.%d", i)

			shell.Shell(
				fmt.Sprintf("find %s | cpio %s -o > %s", d, shellx.VerbosityDefault(), cpio),
			)
			shell.Shell(
				fmt.Sprintf(
					"sed %s -i \"/\\/nix\\/store\\/.\\{32\\}-/s@%s@%s@g\" %s",
					shellx.VerbosityQuietOnlyDefault(),
					sedSearch,
					outDir,
					cpio,
				),
			)
			shell.Shell(
				fmt.Sprintf(
					"(while cpio %s -idm; do :; done) < %s",
					shellx.VerbosityDefault(),
					cpio,
				),
			)
		})

		shell.Shell(fmt.Sprintf("rm %s -rf eject.cpio.*", shellx.VerbosityVerboseOnlyDefault()))
		shell.Shell(fmt.Sprintf("chmod %s -R u+w %s", shellx.VerbosityDefault(), outDir))

		pathsNew := lo.Map(paths, func(p string, _ int) string {
			return lo.T2(
				lo.Must2(
					shell.Shell(
						fmt.Sprintf(
							"sed %s \"s@%s@%s@g\"",
							shellx.VerbosityQuietOnlyDefault(),
							sedSearch,
							outDir,
						),
						shell.WithStdin(p),
					),
				),
			).A
		})

		shell.Shell(
			fmt.Sprintf(
				"cp %s -a %s/home-files/. %s/",
				shellx.VerbosityVerboseOnlyDefault(),
				pathsNew[0],
				environment.Environment.User.HomeDirectory,
			),
		)
		shell.Shell(
			fmt.Sprintf(
				"ln %s -snfF %s %s/.nix-profile",
				shellx.VerbosityVerboseOnlyDefault(),
				pathsNew[1],
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	ejectCmd.Flags().
		StringVar(&outDir, "out-dir", fmt.Sprintf("%s/.nix/e/%s-%s", environment.Environment.User.HomeDirectory, hash.Date(now), hash.TodSeconds(now)), "eject output directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
