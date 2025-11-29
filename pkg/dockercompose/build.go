package dockercompose

import (
	"fmt"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

var DockerCompose = dockerCompose{
	Services: func() map[string]dockerComposeService {
		services := make(map[string]dockerComposeService)
		for _, profile := range profile.HomeManagerProfiles {
			services[fmt.Sprintf("dotfiles-%s", profile)] = dockerComposeService{
				Build: dockerComposeBuild{
					Context:    ".",
					Dockerfile: "docker/Dockerfile",
					Args: map[string]any{
						"PROFILE": profile.String(),
					},
				},
				Image: fmt.Sprintf("dotfiles/%s", profile),
			}
			services[fmt.Sprintf("dotfiles-eject-%s", profile)] = dockerComposeService{
				Build: dockerComposeBuild{
					Context:    ".",
					Dockerfile: "docker/eject.Dockerfile",
					Args: map[string]any{
						"PROFILE": profile.String(),
					},
				},
				Image: fmt.Sprintf("dotfiles-eject/%s", profile),
			}
			// services[fmt.Sprintf("dotfiles-proot-%s", profile)] = dockerComposeService{
			// 	Build: dockerComposeBuild{
			// 		Context:    ".",
			// 		Dockerfile: "docker/proot.Dockerfile",
			// 		Args: map[string]any{
			// 			"PROFILE": profile.String(),
			// 		},
			// 	},
			// 	Image: fmt.Sprintf("dotfiles-proot/%s", profile),
			// }
		}
		return services
	}(),
}
