package themes

import (
	"github.com/hanselrd/dotfiles/pkg/enums"
	"github.com/hanselrd/dotfiles/pkg/structs"
)

var Grayscale = structs.Theme{
	Slug: enums.ThemeGrayscale,
	Palette: structs.ThemePalette{
		Base0:  "111111",
		Base1:  "333333",
		Base2:  "555555",
		Base3:  "666666",
		Base4:  "777777",
		Base5:  "999999",
		Base6:  "BBBBBB",
		Base7:  "DDDDDD",
		Base8:  "333333",
		Base9:  "555555",
		Base10: "777777",
		Base11: "888888",
		Base12: "999999",
		Base13: "BBBBBB",
		Base14: "DDDDDD",
		Base15: "FFFFFF",
	},
}
