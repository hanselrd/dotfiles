let { ThemeSchema, .. } = import "theme.ncl" in
let { UserSchema, .. } = import "user.ncl" in
let { EnumRecord, .. } = import "../contracts/enum-record.ncl" in
let { Role, .. } = import "../enums/role.ncl" in
let { Theme, .. } = import "../enums/theme.ncl" in
{
  EnvironmentSchema = {
    theme | Theme | default = 'grayscale,
    user | UserSchema,
    roles | EnumRecord Role
  }
}
