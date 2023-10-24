package utils

import (
	"fmt"
	"strings"

	sf "github.com/sa-/slicefunk"
)

type choice struct {
	Allowed []string
	Value   string
}

func NewChoice[T fmt.Stringer](allowed []T, value T) *choice {
	return &choice{
		Allowed: sf.Map(
			allowed,
			func(t T) string {
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
