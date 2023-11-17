package utils

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strings"

	"github.com/rs/zerolog/log"

	"github.com/hanselrd/dotfiles/lib/flags"
)

type ShellOpt func(*ShellOpts) error

type ShellOpts struct {
	Stdin string
}

func WithStdin(stdin string) ShellOpt {
	return func(opts *ShellOpts) error {
		opts.Stdin = stdin
		return nil
	}
}

func Shell(command string, opts ...ShellOpt) (stdout string, stderr string, err error) {
	options := ShellOpts{}

	for _, opt := range opts {
		if err = opt(&options); err != nil {
			return
		}
	}

	log.Info().Str("cmdline", command).Bool("dryrun", flags.Dryrun).Send()

	if flags.Dryrun {
		return
	}

	stdoutBuf := new(bytes.Buffer)
	stderrBuf := new(bytes.Buffer)

	cmd := exec.Command("bash", "--norc", "--noprofile", "-c", command)

	if len(options.Stdin) > 0 {
		cmd.Stdin = strings.NewReader(options.Stdin)
	}

	stdoutPipe, _ := cmd.StdoutPipe()
	stderrPipe, _ := cmd.StderrPipe()

	err = cmd.Start()
	if err != nil {
		log.Debug().Msgf("could not start command: \"%s\"", command)
		return
	}

	go scan("cmdout", stdoutPipe, stdoutBuf)
	go scan("cmderr", stderrPipe, stderrBuf)

	err = cmd.Wait()

	stdout = strings.TrimSpace(stdoutBuf.String())
	stderr = strings.TrimSpace(stderrBuf.String())

	return
}

func scan(name string, pipe io.ReadCloser, buffer *bytes.Buffer) {
	scanner := bufio.NewScanner(pipe)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		text := scanner.Text()
		log.Debug().Str(name, text).Send()
		buffer.WriteString(fmt.Sprintln(text))
	}
}
