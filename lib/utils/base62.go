package utils

import (
	"fmt"
	"math/big"
)

func MarshalBase62(u uint64) string {
	var i big.Int
	i.SetUint64(u)
	return i.Text(62)
}

func UnmarshalBase62(s string) (uint64, error) {
	var i big.Int
	_, ok := i.SetString(s, 62)
	if !ok {
		return 0, fmt.Errorf("cannot base62 unmarshal: %q", s)
	}
	return i.Uint64(), nil
}
