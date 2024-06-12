package hash

import (
	"math/big"
	"time"
)

func tod(t time.Time) time.Duration {
	year, month, day := t.Date()
	return t.Sub(time.Date(year, month, day, 0, 0, 0, 0, t.Location()))
}

func TodHours(t time.Time) string {
	return big.NewInt(int64(tod(t).Hours())).Text(36)
}

func TodMinutes(t time.Time) string {
	return big.NewInt(int64(tod(t).Minutes())).Text(36)
}

func TodSeconds(t time.Time) string {
	return big.NewInt(int64(tod(t).Seconds())).Text(36)
}

func TodMilliseconds(t time.Time) string {
	return big.NewInt(tod(t).Milliseconds()).Text(36)
}

func TodMicroseconds(t time.Time) string {
	return big.NewInt(tod(t).Microseconds()).Text(36)
}

func TodNanoseconds(t time.Time) string {
	return big.NewInt(tod(t).Nanoseconds()).Text(36)
}

func Date(t time.Time) string {
	year, month, day := t.Date()
	return big.NewInt(int64((year * 10_000) + (int(month) * 100) + day)).Text(36)
}
