package homemanager

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/hash"
	"github.com/hanselrd/dotfiles/internal/nix"
	"github.com/hanselrd/dotfiles/internal/shell"
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
			slog.Error("", "error", err)
		}
		return
	},
	Run: func(cmd *cobra.Command, args []string) {
		nix.BuildHomeManagerConfiguration(profileGroup)

		paths := lo.Must(nix.FindHomeManagerEjectPaths(profileGroup))
		deps := lo.Must(nix.FindHomeManagerEjectDependencies(profileGroup))

		if !strings.HasSuffix(outDir, "/") {
			outDir += "/"
		}

		sedSearch := fmt.Sprintf("/nix/store/%s-", strings.Repeat(".", 32))[:len(outDir)]

		err := os.MkdirAll(outDir, 0o700)
		cobra.CheckErr(err)

		tmpDir := lo.Must(os.MkdirTemp("", "nix-e-*"))
		defer os.RemoveAll(tmpDir)

		lop.ForEach(deps, func(d string, i int) {
			cpio := filepath.Join(tmpDir, fmt.Sprintf("eject.cpio.%d", i))
			slog.Debug("archiving", "in", d, "out", cpio)

			shell.Shell(
				fmt.Sprintf(
					"find %s | cpio {{.VerbosityQuietLongVerboseShortN}} -o > %s",
					d,
					cpio,
				),
			)
			shell.Shell(
				fmt.Sprintf(
					"sed {{.VerbosityQuietLong}} -i \"/\\/nix\\/store\\/.\\{32\\}-/s@%s@%s@g\" %s",
					sedSearch,
					outDir,
					cpio,
				),
			)
			shell.Shell(
				fmt.Sprintf(
					"(while cpio {{.VerbosityQuietLongVerboseShortN}} -idm; do :; done) < %s",
					cpio,
				),
			)
		})

		shell.Shell(fmt.Sprintf("chmod {{.VerbosityQuietLongVerboseShortN}} -R u+w %s", outDir))

		pathsNew := lo.Map(paths, func(p string, _ int) string {
			return lo.Must(
				shell.Shell(
					fmt.Sprintf(
						"sed {{.VerbosityQuietLong}} \"s@%s@%s@g\"",
						sedSearch,
						outDir,
					),
					shell.WithStdin(p),
				)).Stdout
		})

		shell.Shell(
			fmt.Sprintf(
				"cp {{.VerbosityVerboseShortN}} -a %s/home-files/. %s/",
				pathsNew[0],
				environment.Environment.User.HomeDirectory,
			),
		)
		shell.Shell(
			fmt.Sprintf(
				"ln {{.VerbosityVerboseShortN}} -snfF %s %s/.nix-profile",
				pathsNew[1],
				environment.Environment.User.HomeDirectory,
			),
		)
	},
}

func init() {
	ejectCmd.Flags().
		StringVar(&outDir, "out-dir", fmt.Sprintf("%s/.nix-e/%s-%s", environment.Environment.User.HomeDirectory, hash.Date(now), hash.TodSeconds(now)), "eject output directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
