package assets

import "embed"

var (
	//go:embed templates
	TemplatesFS embed.FS
)
