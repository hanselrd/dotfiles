package shell

import (
	"fmt"
	"strings"
)

type CommandBuilder struct {
	strs []string
}

func (cb *CommandBuilder) Command(cmd string) *CommandBuilder {
	cb.strs = append(cb.strs, cmd)
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

func (cb *CommandBuilder) operators(op string, cmds []string) *CommandBuilder {
	if len(cb.strs) == 0 {
		panic(cb)
	}
	cb.strs[len(cb.strs)-1] += fmt.Sprintf(
		" %s %s",
		op,
		strings.Join(cmds, fmt.Sprintf(" %s ", op)),
	)
	return cb
}

func (cb *CommandBuilder) operator(op, cmd string) *CommandBuilder {
	return cb.operators(op, []string{cmd})
}

func (cb *CommandBuilder) Group(cmds []string) *CommandBuilder {
	cb.strs = append(cb.strs, fmt.Sprintf("{ %s; }", strings.Join(cmds, "; ")))
	return cb
}

func (cb *CommandBuilder) Subgroup(cmds []string) *CommandBuilder {
	cb.strs = append(cb.strs, fmt.Sprintf("(%s)", strings.Join(cmds, "; ")))
	return cb
}

func (cb *CommandBuilder) String() string {
	return strings.Join(cb.strs, "; ")
}
