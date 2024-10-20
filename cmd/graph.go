package cmd

import (
	"bytes"
	"context"
	"fmt"
	"log/slog"
	"path/filepath"
	"strings"

	"github.com/goccy/go-graphviz"
	"github.com/goccy/go-graphviz/cgraph"
	"github.com/samber/lo"
	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/internal/accesslevel"
	"github.com/hanselrd/dotfiles/internal/shell"
	"github.com/hanselrd/dotfiles/pkg/profile"
	"github.com/hanselrd/dotfiles/pkg/role"
)

var graphCmd = &cobra.Command{
	Use:   "graph",
	Short: "Graph command",
	Long:  "Graph command",
	Run: func(cmd *cobra.Command, args []string) {
		ctx := context.Background()
		g := lo.Must(graphviz.New(ctx))
		graph := lo.Must(g.Graph())
		defer func() {
			lo.Must0(graph.Close())
			g.Close()
		}()

		graph.SetLabelLocation(cgraph.TopLocation)
		graph.SetLabel("Dotfiles Profile(s)/Role(s) Architecture")

		profileNodeMap := make(map[string]map[string]*cgraph.Node)
		profileNodeMap[profile.SystemProfileValues()[0].Type()] = make(map[string]*cgraph.Node)
		profileNodeMap[profile.UserProfileValues()[0].Type()] = make(map[string]*cgraph.Node)

		for _, profiles := range [][]profile.Profile{
			lo.Map(
				profile.SystemProfileValues(),
				func(p profile.SystemProfile, _ int) profile.Profile { return p },
			),
			lo.Map(
				profile.UserProfileValues(),
				func(p profile.UserProfile, _ int) profile.Profile { return p },
			),
		} {
			profileNodeMap[profiles[0].Type()] = make(map[string]*cgraph.Node)

			lo.ForEach(profiles, func(p profile.Profile, _ int) {
				nodeName := fmt.Sprintf("profiles.%s.%s", p.Type(), p)
				slog.Debug(
					"creating profile node",
					"name",
					nodeName,
					"type",
					p.Type(),
				)
				node := lo.Must(graph.CreateNodeByName(nodeName))
				node.SetShape(
					lo.Ternary(p.Type() == "system", cgraph.DoubleCircleShape, cgraph.CircleShape),
				)
				node.SetStyle(cgraph.FilledNodeStyle)
				node.SetColor("#F4F4F6")
				profileNodeMap[p.Type()][p.String()] = node
			})

			lo.ForEach(profiles, func(p profile.Profile, _ int) {
				if res, err := shell.Shell(fmt.Sprintf(`git grep -l "\./%s\.nix"`, p)); err == nil {
					lo.ForEach(
						strings.Split(res.Stdout, "\n"),
						func(d string, _ int) {
							d = filepath.Base(d)
							d = strings.TrimSuffix(d, filepath.Ext(d))
							edgeName := fmt.Sprintf(
								"profiles.%s.%s -> profiles.%s.%s",
								p.Type(),
								d,
								p.Type(),
								p,
							)
							slog.Debug(
								"creating profile edge",
								"name",
								edgeName,
								"type",
								p.Type(),
								"start",
								strings.Split(edgeName, " -> ")[0],
								"end",
								strings.Split(edgeName, " -> ")[1],
							)
							lo.Must(
								graph.CreateEdgeByName(
									edgeName,
									profileNodeMap[p.Type()][d],
									profileNodeMap[p.Type()][p.String()],
								),
							)
						})
				}
			})
		}

		roleNodeMap := make(map[string]map[string]*cgraph.Node)
		roleNodeMap[role.SystemRoleValues()[0].Type()] = make(map[string]*cgraph.Node)
		roleNodeMap[role.UserRoleValues()[0].Type()] = make(map[string]*cgraph.Node)

		for _, roles := range [][]role.Role{
			lo.Map(
				role.SystemRoleValues(),
				func(r role.SystemRole, _ int) role.Role { return r },
			),
			lo.Map(
				role.UserRoleValues(),
				func(r role.UserRole, _ int) role.Role { return r },
			),
		} {
			roleNodeMap[roles[0].Type()] = make(map[string]*cgraph.Node)

			lo.ForEach(roles, func(r role.Role, _ int) {
				nodeName := fmt.Sprintf("roles.%s.%s", r.Type(), r)
				slog.Debug(
					"creating role node",
					"name",
					nodeName,
					"type",
					r.Type(),
				)
				node := lo.Must(graph.CreateNodeByName(nodeName))
				node.SetShape(
					lo.Ternary(r.Type() == "system", cgraph.DoubleCircleShape, cgraph.CircleShape),
				)
				node.SetStyle(cgraph.FilledNodeStyle)
				node.SetColor(
					lo.Ternary(
						r.AccessLevel() == accesslevel.AccessLevelPublic,
						"#E6E6E9",
						"#9999A1",
					),
				)
				roleNodeMap[r.Type()][r.String()] = node
			})

			lo.ForEach(roles, func(r role.Role, _ int) {
				if res, err := shell.Shell(fmt.Sprintf(`git grep -l "roles\.%s\.%s\.enable = true"`, r.Type(), r)); err == nil {
					lo.ForEach(
						strings.Split(res.Stdout, "\n"),
						func(d string, _ int) {
							split := strings.Split(d, "/")
							d = filepath.Base(split[2])
							d = strings.TrimSuffix(d, filepath.Ext(d))
							edgeName := fmt.Sprintf(
								"%s.%s.%s -> roles.%s.%s",
								split[1],
								split[0],
								d,
								r.Type(),
								r,
							)
							slog.Debug(
								"creating role edge",
								"name",
								edgeName,
								"type",
								r.Type(),
								"start",
								strings.Split(edgeName, " -> ")[0],
								"end",
								strings.Split(edgeName, " -> ")[1],
							)
							lo.Must(
								graph.CreateEdgeByName(
									edgeName,
									lo.Ternary(split[1] == "profiles", profileNodeMap[split[0]], roleNodeMap[split[0]])[d],
									roleNodeMap[r.Type()][r.String()],
								),
							)
						})
				}
			})
		}

		buf := new(bytes.Buffer)
		lo.Must0(g.Render(ctx, graph, "dot", buf))
		fmt.Println(buf.String())
	},
}

func init() {
	rootCmd.AddCommand(graphCmd)
}
