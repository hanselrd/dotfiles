package lib

import (
	"bytes"
	"log"
	"math"
	"math/rand"
	"strings"

	"github.com/bitfield/script"
)

const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

var (
	alphabetIdxBits = int(math.Log2(float64(len(alphabet))))
	alphabetIdxMask = 1<<alphabetIdxBits - 1
	alphabetIdxMax  = 63 / alphabetIdxBits
)

func RandSeq(n int) (result string) {
	b := make([]byte, n)

	for i, cache, remain := n-1, rand.Int63(), alphabetIdxMax; i >= 0; {
		if remain == 0 {
			cache, remain = rand.Int63(), alphabetIdxMax
		}

		if idx := int(cache) & alphabetIdxMask; idx < len(alphabet) {
			b[i] = alphabet[idx]
			i--
		}

		cache >>= alphabetIdxBits
		remain--
	}

	result = string(b)

	return
}

type ShellOpts struct {
	Input string
}

func Shell(cmd string, dryrun bool, opts *ShellOpts) (rc int, stdout string, stderr string) {
	log.Printf("cmd=%v", cmd)

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
	log.Printf("stdout=%v", stdout)

	stderr = strings.TrimSpace(stderrBuf.String())
	log.Printf("stderr=%v", stderr)

	return
}
