package profiles

import (
	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/structs"
)

var (
	NixOSBase = *structs.NewProfile(
		enums.SystemProfileNixOS,
		enums.UserProfileBase,
	)
	NixOSStandard = *structs.NewProfile(
		enums.SystemProfileNixOS,
		enums.UserProfileStandard,
	)
	NixOSMinimal = *structs.NewProfile(
		enums.SystemProfileNixOS,
		enums.UserProfileMinimal,
	)
	NixOSFull = *structs.NewProfile(
		enums.SystemProfileNixOS,
		enums.UserProfileFull,
	)
)

var NixOSProfiles = [...]structs.Profile{
	NixOSBase,
	NixOSStandard,
	NixOSMinimal,
	NixOSFull,
}
