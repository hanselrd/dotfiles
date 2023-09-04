let { UserSchema, .. } = import "user-schema.ncl" in
let { EnumRecord, .. } = import "enum-record.ncl" in
let { Role, .. } = import "role.ncl" in
{
  EnvironmentSchema = {
    user | UserSchema,
    roles | EnumRecord Role,
  }
}
