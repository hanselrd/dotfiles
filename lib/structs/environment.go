package structs

import (
	"github.com/hanselrd/dotfiles/lib/enums"
)

type Environment struct {
	User    User                           `json:"user"`
	Theme   Theme                          `json:"theme"`
	Roles   map[enums.Role]map[string]bool `json:"roles"`
	Profile Profile                        `json:"profile"`
}
