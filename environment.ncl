let { EnvironmentSchema, .. } = import "lib/environment-schema.ncl" in
let enum_record = import "lib/enum-record.ncl" in
{
  user = {
    username = "delacruz",
    name = "Hansel De La Cruz",
    email = "18725263+hanselrd@users.noreply.github.com"
  },
  roles =
    {}
    |> enum_record.insert
      'homeage
      { secrets = false }
    |> enum_record.insert
      'nix
      { sandbox = true }
    |> enum_record.insert
      'shell
      {
        bashToZsh = false,
        ldPreload = false,
        theme = false
      }
} | EnvironmentSchema
