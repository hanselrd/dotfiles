package profiles

import (
	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/structs"
)

var (
	MacOSBase = *structs.NewProfile(
		enums.SystemProfileMacOS,
		enums.UserProfileBase,
	)
	MacOSStandard = *structs.NewProfile(
		enums.SystemProfileMacOS,
		enums.UserProfileStandard,
	)
	MacOSMinimal = *structs.NewProfile(
		enums.SystemProfileMacOS,
		enums.UserProfileMinimal,
	)
	MacOSFull = *structs.NewProfile(
		enums.SystemProfileMacOS,
		enums.UserProfileFull,
	)
)

var DarwinProfiles = []structs.Profile{
	MacOSBase,
	MacOSStandard,
	MacOSMinimal,
	MacOSFull,
}
