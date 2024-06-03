package enums

type Theme uint

//go:generate go run github.com/dmarkham/enumer -type Theme -trimprefix Theme -linecomment -json -text -transform kebab

const (
	ThemeChalk Theme = iota
	ThemeGrayscale
	ThemeMatrix
	ThemeMocha
	ThemeOcean
	ThemeOneDark
	ThemeTomorrow
	ThemeTwilight
)
