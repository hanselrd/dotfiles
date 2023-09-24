package enums

import "encoding/json"

type Theme uint

//go:generate stringer -type Theme -linecomment

const (
	ThemeChalk     Theme = iota // chalk
	ThemeGrayscale              // grayscale
	ThemeMatrix                 // matrix
	ThemeMocha                  // mocha
	ThemeOcean                  // ocean
	ThemeOneDark                // one-dark
	ThemeTomorrow               // tomorrow
	ThemeTwilight               // twilight
)

func (t Theme) MarshalText() ([]byte, error) {
	return []byte(t.String()), nil
}

func (t Theme) MarshalJSON() ([]byte, error) {
	return json.Marshal(t.String())
}
