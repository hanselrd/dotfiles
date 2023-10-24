package enums

type Theme uint

//go:generate stringer -type Theme -linecomment
//go:generate go run codegen.go Theme

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
