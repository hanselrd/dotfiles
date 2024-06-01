package profiles

import (
	"slices"
)

var HomeManagerProfiles = slices.Concat(
	DarwinProfiles,
	LinuxProfiles,
	WSLProfiles,
)
