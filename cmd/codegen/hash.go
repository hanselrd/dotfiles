package codegen

import (
	"fmt"
	"log/slog"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/samber/lo"
	lop "github.com/samber/lo/parallel"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/nix"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var hashCmd = &cobra.Command{
	Use:   "hash",
	Short: "Hash command",
	Long:  "Hash command",
	Run: func(cmd *cobra.Command, args []string) {
		res := lo.Must(
			shell.Shell(
				`git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'`,
			),
		)
		lop.ForEach(strings.Split(res.Stdout, "\n"), func(s string, _ int) {
			split := strings.Split(s, ":")
			newHash := strconv.Quote(fmt.Sprintf(`%s`, nix.FakeHash(time.Now())))
			slog.Debug(
				"updating hash",
				"file",
				split[0],
				"line",
				split[1],
				"oldHash",
				split[2],
				"newHash",
				newHash,
			)
			shell.Shell(
				fmt.Sprintf(
					"sed {{.VerbosityQuietLong}} -i '%ss@%s@%s@' %s",
					split[1],
					split[2],
					newHash,
					split[0],
				),
			)
		})

		for _, t2 := range []lo.Tuple2[func(profile.ProfileGroup) (shell.ShellResult, error), profile.ProfileGroup]{
			lo.T2(nix.BuildHomeManagerConfiguration, profile.GenericBase),
			lo.T2(nix.BuildNixOSConfiguration, profile.NixosBase),
			lo.T2(nix.BuildNixOSConfiguration, profile.GarudaBase),
			lo.T2(nix.BuildNixOSConfiguration, profile.WslBase),
			lo.T2(nix.BuildDarwinConfiguration, profile.DarwinBase),
		} {
			for {
				buildConfFn, pg := lo.Unpack2(t2)
				res, err := buildConfFn(pg)
				if err != nil {
					re := regexp.MustCompile(`specified: (.*)\n\s*got:    (.*)\n`)
					matches := re.FindAllStringSubmatch(res.Stderr, -1)
					lop.ForEach(matches, func(m []string, _ int) {
						oldHash, newHash := m[1], m[2]
						slog.Debug("replacing hash", "oldHash", oldHash, "newHash", newHash)
						lo.Must(
							shell.Shell(
								fmt.Sprintf(
									`git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock' | xargs sed {{.VerbosityQuietLong}} -i 's@%s@%s@g'`,
									oldHash,
									newHash,
								),
							),
						)
					})
					continue
				}
				break
			}
		}
	},
}

func init() {
	CodegenCmd.AddCommand(hashCmd)
}
