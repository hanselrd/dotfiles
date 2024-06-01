package utils

import (
	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

type caser struct {
	Lower cases.Caser
	Upper cases.Caser
	Title cases.Caser
}

var Caser = caser{
	Lower: cases.Lower(language.English),
	Upper: cases.Upper(language.English),
	Title: cases.Title(language.English),
}
