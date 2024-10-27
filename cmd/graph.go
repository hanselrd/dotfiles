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
	"github.com/hanselrd/dotfiles/internal/encryption"
	"github.com/hanselrd/dotfiles/internal/privilegelevel"
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

		profileNodeMap := make(map[privilegelevel.PrivilegeLevel]map[profile.Profile]*cgraph.Node)
		for _, pl := range privilegelevel.PrivilegeLevelValues() {
			profileNodeMap[pl] = make(map[profile.Profile]*cgraph.Node)
		}

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
			lo.ForEach(profiles, func(p profile.Profile, _ int) {
				nodeName := fmt.Sprintf("profiles.%s.%s", p.PrivilegeLevel(), p)
				slog.Debug(
					"creating profile node",
					"name",
					nodeName,
					"privilegeLevel",
					p.PrivilegeLevel(),
				)
				node := lo.Must(graph.CreateNodeByName(nodeName))
				switch p.PrivilegeLevel() {
				case privilegelevel.PrivilegeLevelSystem:
					node.SetShape(cgraph.DoubleCircleShape)
				case privilegelevel.PrivilegeLevelUser:
					node.SetShape(cgraph.CircleShape)
				}
				node.SetStyle(cgraph.FilledNodeStyle)
				node.SetColor("#F4F4F6")
				profileNodeMap[p.PrivilegeLevel()][p] = node
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
								p.PrivilegeLevel(),
								d,
								p.PrivilegeLevel(),
								p,
							)
							slog.Debug(
								"creating profile edge",
								"name",
								edgeName,
								"privilegeLevel",
								p.PrivilegeLevel(),
								"start",
								strings.Split(edgeName, " -> ")[0],
								"end",
								strings.Split(edgeName, " -> ")[1],
							)
							lo.Must(
								graph.CreateEdgeByName(
									edgeName,
									profileNodeMap[p.PrivilegeLevel()][lo.Must(profile.NewProfile(p.PrivilegeLevel(), d))],
									profileNodeMap[p.PrivilegeLevel()][p],
								),
							)
						})
				}
			})
		}

		roleNodeMap := make(map[privilegelevel.PrivilegeLevel]map[role.Role]*cgraph.Node)
		for _, pl := range privilegelevel.PrivilegeLevelValues() {
			roleNodeMap[pl] = make(map[role.Role]*cgraph.Node)
		}

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
			lo.ForEach(roles, func(r role.Role, _ int) {
				nodeName := fmt.Sprintf("roles.%s.%s", r.PrivilegeLevel(), r)
				slog.Debug(
					"creating role node",
					"name",
					nodeName,
					"privilegeLevel",
					r.PrivilegeLevel(),
				)
				node := lo.Must(graph.CreateNodeByName(nodeName))
				switch r.PrivilegeLevel() {
				case privilegelevel.PrivilegeLevelSystem:
					node.SetShape(cgraph.DoubleCircleShape)
				case privilegelevel.PrivilegeLevelUser:
					node.SetShape(cgraph.CircleShape)
				}
				node.SetStyle(cgraph.FilledNodeStyle)
				switch r.AccessLevel() {
				case accesslevel.AccessLevelPublic:
					node.SetColor("#E6E6E9")
				case accesslevel.AccessLevelSecret:
					switch r.Encryption() {
					case encryption.EncryptionDefault:
						node.SetColor("#9999A1")
					case encryption.EncryptionPrivate:
						node.SetColor("#AF8F8A")
					}
				}
				roleNodeMap[r.PrivilegeLevel()][r] = node
			})

			lo.ForEach(roles, func(r role.Role, _ int) {
				if res, err := shell.Shell(fmt.Sprintf(`git grep -l "roles\.%s\.%s\.enable = true"`, r.PrivilegeLevel(), r)); err == nil {
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
								r.PrivilegeLevel(),
								r,
							)
							slog.Debug(
								"creating role edge",
								"name",
								edgeName,
								"privilegeLevel",
								r.PrivilegeLevel(),
								"start",
								strings.Split(edgeName, " -> ")[0],
								"end",
								strings.Split(edgeName, " -> ")[1],
							)
							pl := lo.Must(privilegelevel.PrivilegeLevelString(split[0]))
							lo.Must(
								graph.CreateEdgeByName(
									edgeName,
									lo.Ternary(
										split[1] == "profiles",
										profileNodeMap[pl][lo.Must(profile.NewProfile(pl, d))],
										roleNodeMap[pl][lo.Must(role.NewRole(pl, d))],
									),
									roleNodeMap[r.PrivilegeLevel()][r],
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
