package structs

type DockerCompose struct {
	Version  string                          `json:"version,omitempty"`
	Services map[string]DockerComposeService `json:"services"`
}

type DockerComposeService struct {
	Build DockerComposeBuild `json:"build"`
	Image string             `json:"image"`
}

type DockerComposeBuild struct {
	Context    string         `json:"context"`
	Dockerfile string         `json:"dockerfile,omitempty"`
	Args       map[string]any `json:"args,omitempty"`
}
