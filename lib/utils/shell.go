package utils

import (
	"bytes"
	"log"
	"strings"

	"github.com/bitfield/script"
)

type ShellOpts struct {
	Input string
}

func Shell(cmd string, dryrun bool, opts *ShellOpts) (rc int, stdout string, stderr string) {
	log.Printf("cmd=%s dryrun=%v", cmd, dryrun)

	if dryrun {
		return
	}

	stdoutBuf := new(bytes.Buffer)
	stderrBuf := new(bytes.Buffer)

	p := script.NewPipe()

	if opts != nil && len(opts.Input) != 0 {
		p = p.Echo(opts.Input)
	}

	p = p.Exec(cmd).WithStdout(stdoutBuf).WithStderr(stderrBuf)

	if err := p.Error(); err != nil {
		p.SetError(nil)
	}

	p.Stdout()

	rc = p.ExitStatus()
	log.Printf("rc=%v", rc)

	stdout = strings.TrimSpace(stdoutBuf.String())
	log.Printf("stdout=%s", stdout)

	stderr = strings.TrimSpace(stderrBuf.String())
	log.Printf("stderr=%s", stderr)

	return
}
