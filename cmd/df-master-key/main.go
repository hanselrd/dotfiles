package main

import (
	"fmt"
	"os"

	"github.com/hanselrd/dotfiles/internal/log"
	"github.com/hanselrd/dotfiles/internal/shell"
)

func main() {
	log.SetupLogger(log.LevelTrace)

	switch os.Args[1] {
	case "generate":
		shell.Shell(fmt.Sprintf("age-keygen -o %s/master.age", os.Args[2]))
		shell.Shell(
			fmt.Sprintf("age-keygen -y -o %[1]s/master.age.pub %[1]s/master.age", os.Args[2]),
		)
	}
}
