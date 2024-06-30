package shell

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"log/slog"
	"os/exec"
	"strings"
	"sync"
	"text/template"

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
		slog.Info("", "cmdin", options.Stdin)
	}

	tmpl := lo.Must(template.New("").Parse(command))
	data := struct {
		VerbosityQuietLong               string
		VerbosityQuietLongVerboseLong    string
		VerbosityQuietLongVerboseLongN   string
		VerbosityQuietLongVerboseShort   string
		VerbosityQuietLongVerboseShortN  string
		VerbosityQuietShort              string
		VerbosityQuietShortVerboseLong   string
		VerbosityQuietShortVerboseLongN  string
		VerbosityQuietShortVerboseShort  string
		VerbosityQuietShortVerboseShortN string
		VerbosityVerboseLong             string
		VerbosityVerboseLongN            string
		VerbosityVerboseShort            string
		VerbosityVerboseShortN           string
	}{
		verbosityQuietVerbose("--quiet", ""),
		verbosityQuietVerbose("--quiet", "--verbose"),
		verbosityQuietVerboseN("--quiet", "--verbose"),
		verbosityQuietVerbose("--quiet", "-v"),
		verbosityQuietVerboseN("--quiet", "-v"),
		verbosityQuietVerbose("-q", ""),
		verbosityQuietVerbose("-q", "--verbose"),
		verbosityQuietVerboseN("-q", "--verbose"),
		verbosityQuietVerbose("-q", "-v"),
		verbosityQuietVerboseN("-q", "-v"),
		verbosityQuietVerbose("", "--verbose"),
		verbosityQuietVerboseN("", "--verbose"),
		verbosityQuietVerbose("", "-v"),
		verbosityQuietVerboseN("", "-v"),
	}
	commandBuf := new(bytes.Buffer)
	if err = tmpl.Execute(commandBuf, data); err != nil {
		return
	}
	command = commandBuf.String()

	slog.Info("", "cmdline", command)

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

	if err = cmd.Start(); err != nil {
		slog.Debug(fmt.Sprintf("could not start command: \"%s\"", command))
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
		slog.Debug("", name, text)
		buffer.WriteString(fmt.Sprintln(text))
	}
}
