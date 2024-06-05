package choice

import (
	"fmt"
	"strings"

	"github.com/samber/lo"
)

type choice struct {
	Allowed []string
	Value   string
}

func New[T fmt.Stringer](allowed []T, value T) *choice {
	return &choice{
		Allowed: lo.Map(
			allowed,
			func(t T, _ int) string {
				return t.String()
			},
		),
		Value: value.String(),
	}
}

func (c choice) String() string {
	return c.Value
}

func (c *choice) Set(p string) error {
	isIncluded := func(opts []string, value string) bool {
		for _, opt := range opts {
			if value == opt {
				return true
			}
		}
		return false
	}
	if !isIncluded(c.Allowed, p) {
		return fmt.Errorf("%s is not included in %s", p, strings.Join(c.Allowed, ","))
	}
	c.Value = p
	return nil
}

func (c *choice) Type() string {
	return "string"
}
