package shell

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"sync"

	"github.com/rs/zerolog/log"
	"github.com/samber/lo"

	"github.com/hanselrd/dotfiles/pkg/flags"
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

func Shell(command string, opts ...ShellOpt) (stdout, stderr string, err error) {
	options := ShellOpts{}

	for _, opt := range opts {
		if err = opt(&options); err != nil {
			return
		}
	}

	if len(options.Stdin) > 0 {
		log.Info().Str("cmdin", options.Stdin).Send()
	}

	log.Info().Str("cmdline", command).Send()

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

	wg := sync.WaitGroup{}
	for _, t3 := range []lo.Tuple3[string, io.ReadCloser, *bytes.Buffer]{
		lo.T3("cmdout", stdoutPipe, stdoutBuf),
		lo.T3("cmderr", stderrPipe, stderrBuf),
	} {
		wg.Add(1)
		go func(t3 lo.Tuple3[string, io.ReadCloser, *bytes.Buffer]) {
			defer wg.Done()
			scan(lo.Unpack3(t3))
		}(t3)
	}
	wg.Wait()

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
