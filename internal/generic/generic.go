package generic

import "github.com/rs/zerolog/log"

func Must[T any](a T, err error) T {
	if err != nil {
		log.Panic().Err(err).Send()
	}
	return a
}

func Must2[T1, T2 any](a T1, b T2, err error) (T1, T2) {
	if err != nil {
		log.Panic().Err(err).Send()
	}
	return a, b
}

func Must3[T1, T2, T3 any](a T1, b T2, c T3, err error) (T1, T2, T3) {
	if err != nil {
		log.Panic().Err(err).Send()
	}
	return a, b, c
}

func First[T any](first T, _ ...any) T {
	return first
}

func Second[T any](_ any, second T, _ ...any) T {
	return second
}

func Third[T any](_, _ any, third T, _ ...any) T {
	return third
}
