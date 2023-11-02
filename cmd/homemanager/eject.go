package homemanager

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/itchyny/timefmt-go"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var (
	now    = time.Now()
	nowymd = timefmt.Format(now, "%y%m%d")
)

var histDir string

var ejectCmd = &cobra.Command{
	Use:   "eject",
	Short: "Eject command",
	Long:  "Eject command",
	Run: func(cmd *cobra.Command, args []string) {
		utils.Shell(fmt.Sprintf("nix build --no-link .#homeConfigurations.%s.activationPackage", profile))
		_, pkg1, _ := utils.Shell(fmt.Sprintf("nix path-info .#homeConfigurations.%s.activationPackage", profile))
		_, deps1, _ := utils.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg1))

		_, pkg2, _ := utils.Shell(fmt.Sprintf("readlink -f %s/.local/state/nix/profiles/profile", dotfiles.Environment.User.HomeDirectory))
		rc, deps2, _ := utils.Shell(fmt.Sprintf("nix-store -qR %s | xargs -L1 basename", pkg2))
		if rc != 0 {
			cobra.CheckErr(fmt.Errorf("rc=%d", rc))
		}

		utils.Shell("sort | uniq > eject.dep", utils.WithStdin(fmt.Sprintf("%s\n%s", deps1, deps2)))
		utils.Shell("find /nix/store -depth -print | grep -Ff eject.dep | cpio -ov > eject.cpio")

		normalizedHistDir := histDir + strings.Repeat("/", len("/nix/store/")+32-len(histDir))

		utils.Shell(fmt.Sprintf("parallel -a eject.cpio -k --block -1 --pipe-part -q sed \"s@/nix/store/.\\{32\\}-@%s/@g\" > eject.cpio~", normalizedHistDir))
		utils.Shell("mv eject.cpio~ eject.cpio")

		err := os.MkdirAll(histDir, 0700)
		cobra.CheckErr(err)

		utils.Shell("cpio -idmv < eject.cpio")

		_, pkg1New, _ := utils.Shell(fmt.Sprintf("sed \"s@/nix/store/.\\{32\\}-@%s/@g\"", histDir), utils.WithStdin(pkg1))
		utils.Shell(fmt.Sprintf("find %s/home-files/ -type d -exec chmod u+w {} \\;", pkg1New))
		utils.Shell(fmt.Sprintf("cp -av %s/home-files/. %s/", pkg1New, dotfiles.Environment.User.HomeDirectory))

		_, pkg2New, _ := utils.Shell(fmt.Sprintf("sed \"s@/nix/store/.\\{32\\}-@%s/@g\"", histDir), utils.WithStdin(pkg2))
		utils.Shell(fmt.Sprintf("ln -snfF %s %s/.nix-profile", pkg2New, dotfiles.Environment.User.HomeDirectory))
	},
}

func init() {
	ejectCmd.Flags().StringVar(&histDir, "hist-dir", fmt.Sprintf("%s/.nix-hme/%s", dotfiles.Environment.User.HomeDirectory, nowymd), "eject history directory")

	HomeManagerCmd.AddCommand(ejectCmd)
}
