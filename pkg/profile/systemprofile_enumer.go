// Code generated by "enumer -type SystemProfile -trimprefix SystemProfile"; DO NOT EDIT.

package profile

import (
	"fmt"
	"strings"
)

const _SystemProfileName = "NixOSGarudaWslDarwinGeneric"

var _SystemProfileIndex = [...]uint8{0, 5, 11, 14, 20, 27}

const _SystemProfileLowerName = "nixosgarudawsldarwingeneric"

func (i SystemProfile) String() string {
	if i >= SystemProfile(len(_SystemProfileIndex)-1) {
		return fmt.Sprintf("SystemProfile(%d)", i)
	}
	return _SystemProfileName[_SystemProfileIndex[i]:_SystemProfileIndex[i+1]]
}

// An "invalid array index" compiler error signifies that the constant values have changed.
// Re-run the stringer command to generate them again.
func _SystemProfileNoOp() {
	var x [1]struct{}
	_ = x[SystemProfileNixOS-(0)]
	_ = x[SystemProfileGaruda-(1)]
	_ = x[SystemProfileWsl-(2)]
	_ = x[SystemProfileDarwin-(3)]
	_ = x[SystemProfileGeneric-(4)]
}

var _SystemProfileValues = []SystemProfile{SystemProfileNixOS, SystemProfileGaruda, SystemProfileWsl, SystemProfileDarwin, SystemProfileGeneric}

var _SystemProfileNameToValueMap = map[string]SystemProfile{
	_SystemProfileName[0:5]:        SystemProfileNixOS,
	_SystemProfileLowerName[0:5]:   SystemProfileNixOS,
	_SystemProfileName[5:11]:       SystemProfileGaruda,
	_SystemProfileLowerName[5:11]:  SystemProfileGaruda,
	_SystemProfileName[11:14]:      SystemProfileWsl,
	_SystemProfileLowerName[11:14]: SystemProfileWsl,
	_SystemProfileName[14:20]:      SystemProfileDarwin,
	_SystemProfileLowerName[14:20]: SystemProfileDarwin,
	_SystemProfileName[20:27]:      SystemProfileGeneric,
	_SystemProfileLowerName[20:27]: SystemProfileGeneric,
}

var _SystemProfileNames = []string{
	_SystemProfileName[0:5],
	_SystemProfileName[5:11],
	_SystemProfileName[11:14],
	_SystemProfileName[14:20],
	_SystemProfileName[20:27],
}

// SystemProfileString retrieves an enum value from the enum constants string name.
// Throws an error if the param is not part of the enum.
func SystemProfileString(s string) (SystemProfile, error) {
	if val, ok := _SystemProfileNameToValueMap[s]; ok {
		return val, nil
	}

	if val, ok := _SystemProfileNameToValueMap[strings.ToLower(s)]; ok {
		return val, nil
	}
	return 0, fmt.Errorf("%s does not belong to SystemProfile values", s)
}

// SystemProfileValues returns all values of the enum
func SystemProfileValues() []SystemProfile {
	return _SystemProfileValues
}

// SystemProfileStrings returns a slice of all String values of the enum
func SystemProfileStrings() []string {
	strs := make([]string, len(_SystemProfileNames))
	copy(strs, _SystemProfileNames)
	return strs
}

// IsASystemProfile returns "true" if the value is listed in the enum definition. "false" otherwise
func (i SystemProfile) IsASystemProfile() bool {
	for _, v := range _SystemProfileValues {
		if i == v {
			return true
		}
	}
	return false
}
