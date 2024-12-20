// Code generated by "enumer -type Encryption -trimprefix Encryption -linecomment -json -text -transform lower"; DO NOT EDIT.

package encryption

import (
	"encoding/json"
	"fmt"
	"strings"
)

const _EncryptionName = "nonedefaultredyellowblue"

var _EncryptionIndex = [...]uint8{0, 4, 11, 14, 20, 24}

const _EncryptionLowerName = "nonedefaultredyellowblue"

func (i Encryption) String() string {
	if i >= Encryption(len(_EncryptionIndex)-1) {
		return fmt.Sprintf("Encryption(%d)", i)
	}
	return _EncryptionName[_EncryptionIndex[i]:_EncryptionIndex[i+1]]
}

// An "invalid array index" compiler error signifies that the constant values have changed.
// Re-run the stringer command to generate them again.
func _EncryptionNoOp() {
	var x [1]struct{}
	_ = x[EncryptionNone-(0)]
	_ = x[EncryptionDefault-(1)]
	_ = x[EncryptionRed-(2)]
	_ = x[EncryptionYellow-(3)]
	_ = x[EncryptionBlue-(4)]
}

var _EncryptionValues = []Encryption{EncryptionNone, EncryptionDefault, EncryptionRed, EncryptionYellow, EncryptionBlue}

var _EncryptionNameToValueMap = map[string]Encryption{
	_EncryptionName[0:4]:        EncryptionNone,
	_EncryptionLowerName[0:4]:   EncryptionNone,
	_EncryptionName[4:11]:       EncryptionDefault,
	_EncryptionLowerName[4:11]:  EncryptionDefault,
	_EncryptionName[11:14]:      EncryptionRed,
	_EncryptionLowerName[11:14]: EncryptionRed,
	_EncryptionName[14:20]:      EncryptionYellow,
	_EncryptionLowerName[14:20]: EncryptionYellow,
	_EncryptionName[20:24]:      EncryptionBlue,
	_EncryptionLowerName[20:24]: EncryptionBlue,
}

var _EncryptionNames = []string{
	_EncryptionName[0:4],
	_EncryptionName[4:11],
	_EncryptionName[11:14],
	_EncryptionName[14:20],
	_EncryptionName[20:24],
}

// EncryptionString retrieves an enum value from the enum constants string name.
// Throws an error if the param is not part of the enum.
func EncryptionString(s string) (Encryption, error) {
	if val, ok := _EncryptionNameToValueMap[s]; ok {
		return val, nil
	}

	if val, ok := _EncryptionNameToValueMap[strings.ToLower(s)]; ok {
		return val, nil
	}
	return 0, fmt.Errorf("%s does not belong to Encryption values", s)
}

// EncryptionValues returns all values of the enum
func EncryptionValues() []Encryption {
	return _EncryptionValues
}

// EncryptionStrings returns a slice of all String values of the enum
func EncryptionStrings() []string {
	strs := make([]string, len(_EncryptionNames))
	copy(strs, _EncryptionNames)
	return strs
}

// IsAEncryption returns "true" if the value is listed in the enum definition. "false" otherwise
func (i Encryption) IsAEncryption() bool {
	for _, v := range _EncryptionValues {
		if i == v {
			return true
		}
	}
	return false
}

// MarshalJSON implements the json.Marshaler interface for Encryption
func (i Encryption) MarshalJSON() ([]byte, error) {
	return json.Marshal(i.String())
}

// UnmarshalJSON implements the json.Unmarshaler interface for Encryption
func (i *Encryption) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return fmt.Errorf("Encryption should be a string, got %s", data)
	}

	var err error
	*i, err = EncryptionString(s)
	return err
}

// MarshalText implements the encoding.TextMarshaler interface for Encryption
func (i Encryption) MarshalText() ([]byte, error) {
	return []byte(i.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface for Encryption
func (i *Encryption) UnmarshalText(text []byte) error {
	var err error
	*i, err = EncryptionString(string(text))
	return err
}
