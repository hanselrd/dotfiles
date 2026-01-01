package main

import (
	"flag"
	"fmt"
	"log/slog"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/internal/log"
	"github.com/hanselrd/dotfiles/internal/nixinternal"
	"github.com/hanselrd/dotfiles/internal/shell"
)

func main() {
	log.SetupLogger(log.LevelTrace)

	flag.Parse()

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
		newHash := strconv.Quote(fmt.Sprintf(`%s`, nixinternal.FakeHash(time.Now())))
		slog.Debug(
			"replacing hash",
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
				"sed -i %s %s",
				strings.Join(lo.Map(sed, func(s string, _ int) string {
					return fmt.Sprintf("-e '%s'", s)
				}), " "),
				file,
			),
		)
	}

	nixosHosts := strings.Split(
		lo.Must(
			shell.Shell(
				"nix eval .#nixosConfigurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\"",
			),
		).Stdout,
		"\n",
	)
	darwinHosts := strings.Split(
		lo.Must(
			shell.Shell(
				"nix eval .#darwinConfigurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\"",
			),
		).Stdout,
		"\n",
	)
	homes := strings.Split(
		lo.Must(
			shell.Shell(
				"nix eval .#homeConfigurations --apply \"x: builtins.attrNames x\" --json | jq -r \".[]\"",
			),
		).Stdout,
		"\n",
	)
	buildCmds := slices.Concat(
		[]string{"nix run .#update-hashes -- --help"},
		lo.Map(nixosHosts, func(host string, _ int) string {
			return fmt.Sprintf("nh os build . -H %s --impure --no-nom", host)
		}),
		lo.Map(darwinHosts, func(host string, _ int) string {
			return fmt.Sprintf("nh darwin build . -H %s --impure --no-nom", host)
		}),
		lo.Map(homes, func(home string, _ int) string {
			return fmt.Sprintf("nh home build . -c %s --impure --no-nom", home)
		}),
	)
	re := regexp.MustCompile(`specified: (.*)\n\s*got:    (.*)\n`)
	count := len(strings.Split(res.Stdout, "\n"))
	for _, buildCmd := range buildCmds {
		for count > 0 {
			res, err := shell.Shell(buildCmd)
			if err != nil {
				matches := re.FindAllStringSubmatch(res.Stdout+res.Stderr, -1)
				if len(matches) == 0 {
					os.Exit(1)
				}
				lo.ForEach(matches, func(m []string, _ int) {
					oldHash, newHash := m[1], m[2]
					slog.Debug("replacing hash", "old", oldHash, "new", newHash)
					lo.Must(
						shell.Shell(
							fmt.Sprintf(
								`git grep -Pl "[Hh]ash\s*=\s*\K(\"sha256-.{43}=\"|lib\.fakeHash)" -- ':!flake.lock' | xargs sed -i 's@%s@%s@g'`,
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
}
