package asset

import (
	"text/template"

	"github.com/Masterminds/sprig/v3"
	"github.com/samber/lo"
)

var Template = lo.Must(template.New("").
	Funcs(sprig.FuncMap()).
	ParseFS(TemplatesFS, "templates/*.gotmpl"))
