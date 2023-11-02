package utils

import (
	"bytes"
	"log"
	"os/exec"
	"strings"

	expect "github.com/Netflix/go-expect"

	"github.com/hanselrd/dotfiles/lib/flags"
)

type ShellOpt func(*ShellOpts) error

type ExpectFn func(*expect.Console) error

type ShellOpts struct {
	Stdin    string
	ExpectFn ExpectFn
}

func WithStdin(stdin string) ShellOpt {
	return func(opts *ShellOpts) error {
		opts.Stdin = stdin
		return nil
	}
}

func WithExpectFn(fn ExpectFn) ShellOpt {
	return func(opts *ShellOpts) error {
		opts.ExpectFn = fn
		return nil
	}
}

func Shell(command string, opts ...ShellOpt) (rc int, stdout string, stderr string) {
	options := ShellOpts{}

	for _, opt := range opts {
		if err := opt(&options); err != nil {
			return
		}
	}

	log.Printf("command=%s dryrun=%v", command, flags.Dryrun)

	if flags.Dryrun {
		return
	}

	stdoutBuf := new(bytes.Buffer)
	stderrBuf := new(bytes.Buffer)

	// c, err := expect.NewConsole(expect.WithStdout(os.Stdout), expect.WithStdout(stdoutBuf))
	// if err != nil {
	// 	log.Fatal("could not create console")
	// }
	// defer c.Close()

	cmd := exec.Command("bash", "--norc", "--noprofile", "-c", command)
	// cmd.Stdin = c.Tty()
	// cmd.Stdout = c.Tty()
	// cmd.Stderr = c.Tty()

	if len(options.Stdin) > 0 {
		cmd.Stdin = strings.NewReader(options.Stdin)
	}

	cmd.Stdout = stdoutBuf
	cmd.Stderr = stderrBuf

	err := cmd.Start()
	if err != nil {
		log.Fatalf("could not start command: \"%s\"", command)
	}

	// if options.ExpectFn != nil {
	// 	options.ExpectFn(c)
	// }

	err = cmd.Wait()
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			rc = exitError.ExitCode()
		} else {
			log.Fatalf("could not wait on command")
		}
	}

	log.Printf("rc=%v", rc)

	stdout = strings.TrimSpace(stdoutBuf.String())
	log.Printf("stdout=%s", stdout)

	stderr = strings.TrimSpace(stderrBuf.String())
	log.Printf("stderr=%s", stderr)

	return
}
