package codegen

import (
	"fmt"
	"log/slog"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/nixutil"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
)

var hashCmd = &cobra.Command{
	Use:   "hash",
	Short: "Hash command",
	Long:  "Hash command",
	RunE: func(cmd *cobra.Command, args []string) error {
		res := lo.Must(
			shell.Shell(
				`git grep -Po "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock'`,
			),
		)
		seds := make(map[string][]string)
		lo.ForEach(strings.Split(res.Stdout, "\n"), func(s string, _ int) {
			split := strings.Split(s, ":")
			if len(split) != 3 {
				panic(split)
			}
			file, line, oldHash := split[0], split[1], split[2]
			newHash := strconv.Quote(fmt.Sprintf(`%s`, nixutil.FakeHash(time.Now())))
			slog.Debug(
				"updating hash",
				"file",
				file,
				"line",
				line,
				"old",
				oldHash,
				"new",
				newHash,
			)
			seds[file] = append(seds[file], fmt.Sprintf("%ss@%s@%s@", line, oldHash, newHash))
		})
		for file, sed := range seds {
			shell.Shell(
				fmt.Sprintf(
					"sed {{.VerbosityQuietLong}} -i %s %s",
					strings.Join(lo.Map(sed, func(s string, _ int) string {
						return fmt.Sprintf("-e '%s'", s)
					}), " "),
					file,
				),
			)
		}

		re := regexp.MustCompile(`specified: (.*)\n\s*got:    (.*)\n`)
		count := len(strings.Split(res.Stdout, "\n"))
		for _, t2 := range []lo.Tuple2[func(profile.ProfileGroup) (shell.ShellResult, error), profile.ProfileGroup]{
			lo.T2(nixutil.BuildHomeManagerConfiguration, profile.GenericBase),
			lo.T2(nixutil.BuildNixOSConfiguration, profile.NixOSBase),
			lo.T2(nixutil.BuildNixOSConfiguration, profile.GarudaBase),
			lo.T2(nixutil.BuildNixOSConfiguration, profile.WslBase),
			lo.T2(nixutil.BuildDarwinConfiguration, profile.DarwinBase),
		} {
			for count > 0 {
				buildConfFn, pg := lo.Unpack2(t2)
				res, err := buildConfFn(pg)
				if err != nil {
					matches := re.FindAllStringSubmatch(res.Stderr, -1)
					if len(matches) == 0 {
						return err
					}
					lo.ForEach(matches, func(m []string, _ int) {
						oldHash, newHash := m[1], m[2]
						slog.Debug("replacing hash", "old", oldHash, "new", newHash)
						lo.Must(
							shell.Shell(
								fmt.Sprintf(
									`git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock' | xargs sed {{.VerbosityQuietLong}} -i 's@%s@%s@g'`,
									oldHash,
									newHash,
								),
							),
						)
						count--
					})
					continue
				}
				break
			}
		}
		return nil
	},
}

func init() {
	CodegenCmd.AddCommand(hashCmd)
}
