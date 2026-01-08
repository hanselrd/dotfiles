package shell

import (
	"fmt"
	"strings"

	"github.com/samber/lo"
)

type CommandBuilder struct {
	sb strings.Builder
}

func (cb *CommandBuilder) Command(cmd string) *CommandBuilder {
	if cb.sb.Len() > 0 {
		lo.Must(cb.sb.WriteString("; "))
	}
	lo.Must(cb.sb.WriteString(cmd))
	return cb
}

func (cb *CommandBuilder) Pipes(cmds []string) *CommandBuilder {
	return cb.operators("|", cmds)
}

func (cb *CommandBuilder) Pipe(cmd string) *CommandBuilder {
	return cb.operator("|", cmd)
}

func (cb *CommandBuilder) Ands(cmds []string) *CommandBuilder {
	return cb.operators("&&", cmds)
}

func (cb *CommandBuilder) And(cmd string) *CommandBuilder {
	return cb.operator("&&", cmd)
}

func (cb *CommandBuilder) Ors(cmds []string) *CommandBuilder {
	return cb.operators("||", cmds)
}

func (cb *CommandBuilder) Or(cmd string) *CommandBuilder {
	return cb.operator("||", cmd)
}

func (cb *CommandBuilder) Group(cmds []string) *CommandBuilder {
	lo.Must(cb.sb.WriteString(fmt.Sprintf("{ %s; }", strings.Join(cmds, "; "))))
	return cb
}

func (cb *CommandBuilder) Subgroup(cmds []string) *CommandBuilder {
	lo.Must(cb.sb.WriteString(fmt.Sprintf("(%s)", strings.Join(cmds, "; "))))
	return cb
}

func (cb *CommandBuilder) String() string {
	return cb.sb.String()
}

func (cb *CommandBuilder) operators(op string, cmds []string) *CommandBuilder {
	paddedOp := fmt.Sprintf(" %s ", op)
	if cb.sb.Len() > 0 {
		lo.Must(cb.sb.WriteString(paddedOp))
	}
	lo.Must(cb.sb.WriteString(strings.Join(cmds, paddedOp)))
	return cb
}

func (cb *CommandBuilder) operator(op, cmd string) *CommandBuilder {
	if cb.sb.Len() == 0 {
		panic(cb)
	}
	return cb.operators(op, []string{cmd})
}
