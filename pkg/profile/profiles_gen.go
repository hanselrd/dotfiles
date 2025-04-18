// Code generated by "generator"; DO NOT EDIT.
package profile

import "slices"

var (
	NixOSBase       = NewProfileGroup(SystemProfileNixOS, UserProfileBase)
	NixOSMinimal    = NewProfileGroup(SystemProfileNixOS, UserProfileMinimal)
	NixOSStandard   = NewProfileGroup(SystemProfileNixOS, UserProfileStandard)
	NixOSFull       = NewProfileGroup(SystemProfileNixOS, UserProfileFull)
	GarudaBase      = NewProfileGroup(SystemProfileGaruda, UserProfileBase)
	GarudaMinimal   = NewProfileGroup(SystemProfileGaruda, UserProfileMinimal)
	GarudaStandard  = NewProfileGroup(SystemProfileGaruda, UserProfileStandard)
	GarudaFull      = NewProfileGroup(SystemProfileGaruda, UserProfileFull)
	WslBase         = NewProfileGroup(SystemProfileWsl, UserProfileBase)
	WslMinimal      = NewProfileGroup(SystemProfileWsl, UserProfileMinimal)
	WslStandard     = NewProfileGroup(SystemProfileWsl, UserProfileStandard)
	WslFull         = NewProfileGroup(SystemProfileWsl, UserProfileFull)
	DarwinBase      = NewProfileGroup(SystemProfileDarwin, UserProfileBase)
	DarwinMinimal   = NewProfileGroup(SystemProfileDarwin, UserProfileMinimal)
	DarwinStandard  = NewProfileGroup(SystemProfileDarwin, UserProfileStandard)
	DarwinFull      = NewProfileGroup(SystemProfileDarwin, UserProfileFull)
	GenericBase     = NewProfileGroup(SystemProfileGeneric, UserProfileBase)
	GenericMinimal  = NewProfileGroup(SystemProfileGeneric, UserProfileMinimal)
	GenericStandard = NewProfileGroup(SystemProfileGeneric, UserProfileStandard)
	GenericFull     = NewProfileGroup(SystemProfileGeneric, UserProfileFull)
)

var (
	NixOSProfiles       = []ProfileGroup{NixOSBase, NixOSMinimal, NixOSStandard, NixOSFull}
	GarudaProfiles      = []ProfileGroup{GarudaBase, GarudaMinimal, GarudaStandard, GarudaFull}
	WslProfiles         = []ProfileGroup{WslBase, WslMinimal, WslStandard, WslFull}
	DarwinProfiles      = []ProfileGroup{DarwinBase, DarwinMinimal, DarwinStandard, DarwinFull}
	GenericProfiles     = []ProfileGroup{GenericBase, GenericMinimal, GenericStandard, GenericFull}
	HomeManagerProfiles = slices.Concat(NixOSProfiles, GarudaProfiles, WslProfiles, DarwinProfiles, GenericProfiles)
)
