package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"

	"github.com/bitfield/script"
	"github.com/fatih/color"
)

func shell(command string, dryrun bool) (rc int, stdout string, stderr string) {
	log.Printf("command=%v", command)

	if dryrun {
		return
	}

	stdoutBuf := new(bytes.Buffer)
	stderrBuf := new(bytes.Buffer)
	p := script.NewPipe().WithStdout(stdoutBuf).WithStderr(stderrBuf).Exec(command)

	if err := p.Error(); err != nil {
		p.SetError(nil)
	}

	p.Wait()

	rc = p.ExitStatus()
	log.Printf("rc=%v", rc)

	stdout = stdoutBuf.String()
	log.Printf("stdout=%v", stdout)

	stderr = stderrBuf.String()
	log.Printf("stderr=%v", stderr)

	return
}

func main() {
	var configuration string
	flag.StringVar(&configuration, "configuration", "linux-base", "home-manager configuration")

	var eject bool
	flag.BoolVar(&eject, "eject", false, "ejects home-manager configuration to work without nix")

	var dryrun bool
	flag.BoolVar(&dryrun, "dryrun", false, "runs without affecting the system")

	flag.Parse()

	if dryrun {
		c := color.New(color.FgYellow)
		log.SetPrefix(c.Sprint("[DRYRUN] "))
	}

	log.Printf("configuration: %v\n", configuration)
	log.Printf("eject: %v\n", eject)

	shell(fmt.Sprintf("nix build --no-link .#homeConfigurations.%v.activationPackage --impure --extra-experimental-features \"nix-command flakes\" --accept-flake-config", configuration),
		false)

	// cmd := exec.Command(
	// 	"nix",
	// 	"build",
	// 	"--no-link",
	// 	fmt.Sprintf(".#homeConfigurations.%v.activationPackage", configuration),
	// 	"--impure",
	// 	"--extra-experimental-features",
	// 	"nix-command flakes",
	// 	// "flakes",
	// 	// `"nix-command flakes"`,
	// 	"--accept-flake-config",
	// )
	// HMB_HOME_MANAGER="$(nix path-info ".#homeConfigurations.$HMB_CONFIGURATION.activationPackage" --impure --extra-experimental-features "nix-command flakes" --accept-flake-config)"/home-path/bin/home-manager
	// $HMB_HOME_MANAGER switch --flake ".#$HMB_CONFIGURATION" -b bak."$(date +"%Y%m%d")" --impure --extra-experimental-features "nix-command flakes" --option accept-flake-config true
}
