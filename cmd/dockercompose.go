package cmd

import (
	"encoding/json"
	"fmt"

	"github.com/spf13/cobra"

	"github.com/hanselrd/dotfiles/lib/profiles"
	"github.com/hanselrd/dotfiles/lib/structs"
	"github.com/hanselrd/dotfiles/lib/utils"
)

var dockerComposeCmd = &cobra.Command{
	Use:   "dockerCompose",
	Short: "Docker Compose command",
	Long:  "Docker Compose command",
	Run: func(cmd *cobra.Command, args []string) {
		dc := structs.DockerCompose{
			Version: "3",
			Services: func() map[string]structs.DockerComposeService {
				services := make(map[string]structs.DockerComposeService)
				for _, profile := range profiles.HomeManagerProfiles {
					services[fmt.Sprintf("dotfiles-%s", profile)] = structs.DockerComposeService{
						Build: structs.DockerComposeBuild{
							Context:    ".",
							Dockerfile: "docker/Dockerfile",
							Args: map[string]any{
								"PROFILE": profile.String(),
							},
						},
						Image: fmt.Sprintf("dotfiles/%s", profile),
					}
					services[fmt.Sprintf("dotfiles-eject-%s", profile)] = structs.DockerComposeService{
						Build: structs.DockerComposeBuild{
							Context:    ".",
							Dockerfile: "docker/eject.Dockerfile",
							Args: map[string]any{
								"PROFILE": profile.String(),
							},
						},
						Image: fmt.Sprintf("dotfiles-eject/%s", profile),
					}
					services[fmt.Sprintf("dotfiles-proot-%s", profile)] = structs.DockerComposeService{
						Build: structs.DockerComposeBuild{
							Context:    ".",
							Dockerfile: "docker/proot.Dockerfile",
							Args: map[string]any{
								"PROFILE": profile.String(),
							},
						},
						Image: fmt.Sprintf("dotfiles-proot/%s", profile),
					}
				}
				return services
			}(),
		}

		data := utils.Must(json.MarshalIndent(dc, "", "  "))
		fmt.Println(string(data))
	},
}

func init() {
	rootCmd.AddCommand(dockerComposeCmd)
}
