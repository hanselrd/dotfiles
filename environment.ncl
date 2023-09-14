let { EnvironmentSchema, .. } = import "lib/schemas/environment.ncl" in
let enum_record = import "lib/contracts/enum-record.ncl" in
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
      { decrypt = true }
    |> enum_record.insert
      'nix
      { sandbox = true }
    |> enum_record.insert
      'shell
      {
        bashToZsh = false,
        ldPreload = false,
        rts = false,
        theme = false
      }
} | EnvironmentSchema
