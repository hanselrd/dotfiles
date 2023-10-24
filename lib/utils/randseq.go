package utils

import (
	"math"
	"math/rand"
)

const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

var (
	alphabetIdxBits = int(math.Log2(float64(len(alphabet))))
	alphabetIdxMask = 1<<alphabetIdxBits - 1
	alphabetIdxMax  = 63 / alphabetIdxBits
)

func RandSeq(n int) (result string) {
	b := make([]byte, n)

	for i, cache, remain := n-1, rand.Int63(), alphabetIdxMax; i >= 0; {
		if remain == 0 {
			cache, remain = rand.Int63(), alphabetIdxMax
		}

		if idx := int(cache) & alphabetIdxMask; idx < len(alphabet) {
			b[i] = alphabet[idx]
			i--
		}

		cache >>= alphabetIdxBits
		remain--
	}

	result = string(b)

	return
}
