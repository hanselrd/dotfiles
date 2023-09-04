let { UserSchema, .. } = import "user.ncl" in
let { EnumRecord, .. } = import "../contracts/enum-record.ncl" in
let { Role, .. } = import "../enums/role.ncl" in
let { System, .. } = import "../enums/system.ncl" in
{
  EnvironmentSchema = {
    _system
      | System
      | default
      = 'x86_64-linux,
    user | UserSchema,
    roles | EnumRecord Role
  }
}
