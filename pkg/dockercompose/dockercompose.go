package dockercompose

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
