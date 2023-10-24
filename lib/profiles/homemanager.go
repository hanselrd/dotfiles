package profiles

import (
	"github.com/hanselrd/dotfiles/lib/enums"
	"github.com/hanselrd/dotfiles/lib/structs"
)

var (
	LinuxSystemdBase = *structs.NewProfile(
		enums.SystemProfileLinuxSystemd,
		enums.UserProfileBase,
	)
	LinuxSystemdStandard = *structs.NewProfile(
		enums.SystemProfileLinuxSystemd,
		enums.UserProfileStandard,
	)
	LinuxSystemdMinimal = *structs.NewProfile(
		enums.SystemProfileLinuxSystemd,
		enums.UserProfileMinimal,
	)
	LinuxSystemdFull = *structs.NewProfile(
		enums.SystemProfileLinuxSystemd,
		enums.UserProfileFull,
	)
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
	LinuxSystemdBase,
	LinuxSystemdStandard,
	LinuxSystemdMinimal,
	LinuxSystemdFull,
	LinuxBase,
	LinuxStandard,
	LinuxMinimal,
	LinuxFull,
	WSLBase,
	WSLStandard,
	WSLMinimal,
	WSLFull,
}
