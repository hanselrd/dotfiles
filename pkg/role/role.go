package role

import "fmt"

type Role interface {
	fmt.Stringer
	Type() string
}
