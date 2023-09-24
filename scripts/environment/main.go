package main

import (
	"encoding/json"
	"fmt"

	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/profiles"
	"github.com/hanselrd/dotfiles/lib/structs"
	"github.com/hanselrd/dotfiles/lib/themes"
)

func main() {
	e := structs.Environment{
		User: *structs.NewUser(
			"delacruz",
			"Hansel De La Cruz",
			"18725263+hanselrd@users.noreply.github.com",
		),
		Theme: themes.Chalk,
		Roles: map[enums.Role]map[string]bool{
			enums.RoleHomeage: {
				"decrypt": false,
			},
			enums.RoleNix: {
				"sandbox": true,
			},
			enums.RoleShell: {
				"bashToZsh": false,
				"ldPreload": false,
				"rts":       false,
				"theme":     false,
			},
		},
		Profile: profiles.NixOSStandard,
	}
	data, _ := json.Marshal(e)
	fmt.Println(string(data))
}
