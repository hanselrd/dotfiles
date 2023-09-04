{
  UserSchema = {
    username | String,
    name | String,
    email | String,
    homeDirectory | String | default = "/home/%{username}",
    configDirectory
      | String
      | doc "Where user-specific configurations should be written (analogous to /etc)"
      | default
      = "%{homeDirectory}/.config",
    cacheDirectory
      | String
      | doc "Where user-specific non-essential (cached) data should be written (analogous to /var/cache)"
      | default
      = "%{homeDirectory}/.cache",
    dataDirectory
      | String
      | doc "Where user-specific data files should be written (analogous to /usr/share)"
      | default
      = "%{homeDirectory}/.local/share",
    stateDirectory
      | String
      | doc "Where user-specific state files should be written (analogous to /var/lib)"
      | default
      = "%{homeDirectory}/.local/state"
  }
}
