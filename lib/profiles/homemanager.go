package profiles

import (
	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/structs"
)

var (
	LinuxBase = *structs.NewProfile(
		enums.SystemProfileLinux,
		enums.UserProfileBase,
	)
	LinuxStandard = *structs.NewProfile(
		enums.SystemProfileLinux,
		enums.UserProfileStandard,
	)
	LinuxMinimal = *structs.NewProfile(
		enums.SystemProfileLinux,
		enums.UserProfileMinimal,
	)
	LinuxFull = *structs.NewProfile(
		enums.SystemProfileLinux,
		enums.UserProfileFull,
	)
	WSLBase = *structs.NewProfile(
		enums.SystemProfileWSL,
		enums.UserProfileBase,
	)
	WSLStandard = *structs.NewProfile(
		enums.SystemProfileWSL,
		enums.UserProfileStandard,
	)
	WSLMinimal = *structs.NewProfile(
		enums.SystemProfileWSL,
		enums.UserProfileMinimal,
	)
	WSLFull = *structs.NewProfile(
		enums.SystemProfileWSL,
		enums.UserProfileFull,
	)
)

var HomeManagerProfiles = []structs.Profile{
	LinuxBase,
	LinuxStandard,
	LinuxMinimal,
	LinuxFull,
	WSLBase,
	WSLStandard,
	WSLMinimal,
	WSLFull,
}
