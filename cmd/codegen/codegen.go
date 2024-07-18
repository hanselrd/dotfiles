package codegen

import (
	"log/slog"
	"text/template"

	"github.com/iancoleman/strcase"
	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/assets"
)

var tmpl *template.Template

var CodegenCmd = &cobra.Command{
	Use:   "codegen",
	Short: "Codegen command",
	Long:  "Codegen command",
	Run: func(cmd *cobra.Command, args []string) {
		slog.Info("codegen called")
	},
}

func init() {
	tmpl = lo.Must(template.New("").Funcs(template.FuncMap{
		"camel":      strcase.ToCamel,
		"lowerCamel": strcase.ToLowerCamel,
	}).ParseFS(assets.TemplatesFS, "templates/*.gotmpl"))
}
