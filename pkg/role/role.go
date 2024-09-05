package role

import (
	"fmt"

	"github.com/hanselrd/dotfiles/internal/accesslevel"
)

type Role interface {
	fmt.Stringer
	Type() string
	AccessLevel() accesslevel.AccessLevel
}
