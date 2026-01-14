package shell

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCommand(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("touch file.txt").String()
	assert.Equal(t, "touch file.txt", cmd, "")
}

func TestCommands(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("touch file.txt").
		Command("file file.txt").
		Command("cat file.txt").
		String()
	assert.Equal(t, "touch file.txt; file file.txt; cat file.txt", cmd, "")
}

func TestPipe(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Pipe("wc -l").
		String()
	assert.Equal(t, "cat file.txt | wc -l", cmd, "")
}

func TestPipes(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Pipes([]string{
			"sed -E 's/one/two/g'",
			"wc -l",
		}).
		String()
	assert.Equal(t, "cat file.txt | sed -E 's/one/two/g' | wc -l", cmd, "")
}

func TestAnd(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		And("wc -l").
		String()
	assert.Equal(t, "cat file.txt && wc -l", cmd, "")
}

func TestAnds(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Ands([]string{
			"sed -E 's/one/two/g'",
			"wc -l",
		}).
		String()
	assert.Equal(t, "cat file.txt && sed -E 's/one/two/g' && wc -l", cmd, "")
}

func TestOr(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Or("wc -l").
		String()
	assert.Equal(t, "cat file.txt || wc -l", cmd, "")
}

func TestOrs(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Ors([]string{
			"sed -E 's/one/two/g'",
			"wc -l",
		}).
		String()
	assert.Equal(t, "cat file.txt || sed -E 's/one/two/g' || wc -l", cmd, "")
}

func TestStdin(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("wc -l").
		Stdin("file.txt").
		String()
	assert.Equal(t, "wc -l < file.txt", cmd, "")
}

func TestStdout(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Stdout("file2.txt").
		String()
	assert.Equal(t, "cat file.txt > file2.txt", cmd, "")
}

func TestStdoutAppend(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		StdoutAppend("file2.txt").
		String()
	assert.Equal(t, "cat file.txt >> file2.txt", cmd, "")
}

func TestStderr(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		Stderr("file2.txt").
		String()
	assert.Equal(t, "cat file.txt 2> file2.txt", cmd, "")
}

func TestStderrAppend(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		StderrAppend("file2.txt").
		String()
	assert.Equal(t, "cat file.txt 2>> file2.txt", cmd, "")
}

func TestStdoutStderr(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		StdoutStderr("file2.txt").
		String()
	assert.Equal(t, "cat file.txt &> file2.txt", cmd, "")
}

func TestStdoutStderrAppend(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Command("cat file.txt").
		StdoutStderrAppend("file2.txt").
		String()
	assert.Equal(t, "cat file.txt &>> file2.txt", cmd, "")
}

func TestGroup(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Group([]string{
		"touch file.txt",
		"file file.txt",
		"cat file.txt",
	}).String()
	assert.Equal(t, "{ touch file.txt; file file.txt; cat file.txt; }", cmd, "")
}

func TestSubgroup(t *testing.T) {
	var cb CommandBuilder
	cmd := cb.Subgroup([]string{
		"touch file.txt",
		"file file.txt",
		"cat file.txt",
	}).String()
	assert.Equal(t, "(touch file.txt; file file.txt; cat file.txt)", cmd, "")
}
