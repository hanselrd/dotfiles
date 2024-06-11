package profile

import "fmt"

type Profile interface {
	fmt.Stringer
	Type() string
}
