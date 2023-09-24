package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/fatih/color"
	"github.com/itchyny/timefmt-go"

	"github.com/hanselrd/dotfiles/lib"
)

func main() {
	now := time.Now()
	nowYmd := timefmt.Format(now, "%Y%m%d")
	nowYmdTHMS := timefmt.Format(now, "%Y%m%dT%H%M%S")

	var preset string
	flag.StringVar(&preset, "preset", "linux-base", "home-manager preset")

	var eject bool
	flag.BoolVar(&eject, "eject", false, "ejects home-manager configuration to work without nix")

	var dryrun bool
	flag.BoolVar(&dryrun, "dryrun", false, "runs without affecting the system")

	defaultTempDir, err := os.MkdirTemp("", "home-manager")
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(defaultTempDir)
	var tempDir string
	flag.StringVar(&tempDir, "temp-dir", defaultTempDir, "Temporary directory")

	var homeDir string
	flag.StringVar(&homeDir, "home-dir", os.Getenv("HOME"), "Home directory")

	var hash = lib.RandSeq(5)

	var histDir string
	flag.StringVar(&histDir, "hist-dir", fmt.Sprintf("%v/.nix/hm-store/%v-%v", homeDir, nowYmdTHMS, hash), "Home directory")

	var storeDir string
	flag.StringVar(&storeDir, "store-dir", fmt.Sprintf("/tmp/%v", hash), "Store directory")

	flag.Parse()

	if dryrun {
		c := color.New(color.FgYellow)
		log.SetPrefix(c.Sprint("[DRYRUN] "))
	}

	// if !dryrun {
	// 	if err := os.Link(histDir, storeDir); err != nil {
	// 		log.Fatal(err)
	// 	}
	// }

	lib.Shell(
		fmt.Sprintf("nix build --no-link .#homeConfigurations.%v.activationPackage", preset),
		dryrun, nil)
	_, stdout, _ := lib.Shell(
		fmt.Sprintf("nix path-info .#homeConfigurations.%v.activationPackage", preset),
		dryrun, nil)

	homeManagerExe := fmt.Sprintf("%v/home-path/bin/home-manager", stdout)
	lib.Shell(
		fmt.Sprintf("%v switch --flake .#%v -b bak.%v", homeManagerExe, preset, nowYmd),
		dryrun, nil)
}
