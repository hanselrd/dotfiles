package structs

import (
	"github.com/hanselrd/dotfiles/lib/enums"
)

type Theme struct {
	Slug    enums.Theme  `json:"slug"`
	Name    string       `json:"name,omitempty"`
	Email   string       `json:"author,omitempty"`
	Palette ThemePalette `json:"palette"`
}

type ThemePalette struct {
	Base0  string `json:"base00"`
	Base1  string `json:"base01"`
	Base2  string `json:"base02"`
	Base3  string `json:"base03"`
	Base4  string `json:"base04"`
	Base5  string `json:"base05"`
	Base6  string `json:"base06"`
	Base7  string `json:"base07"`
	Base8  string `json:"base08"`
	Base9  string `json:"base09"`
	Base10 string `json:"base0A"`
	Base11 string `json:"base0B"`
	Base12 string `json:"base0C"`
	Base13 string `json:"base0D"`
	Base14 string `json:"base0E"`
	Base15 string `json:"base0F"`
}
