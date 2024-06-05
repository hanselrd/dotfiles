package dockercompose

import (
	"fmt"

	"github.com/hanselrd/dotfiles/pkg/profile"
)

type dockerCompose struct {
	Version  string                          `json:"version,omitempty"`
	Services map[string]dockerComposeService `json:"services"`
}

type dockerComposeService struct {
	Build dockerComposeBuild `json:"build"`
	Image string             `json:"image"`
}

type dockerComposeBuild struct {
	Context    string         `json:"context"`
	Dockerfile string         `json:"dockerfile,omitempty"`
	Args       map[string]any `json:"args,omitempty"`
}

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
			services[fmt.Sprintf("dotfiles-proot-%s", profile)] = dockerComposeService{
				Build: dockerComposeBuild{
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
