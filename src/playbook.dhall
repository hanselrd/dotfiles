let External/Ansible = ./Lib/External/Ansible.partial.dhall

let External/Prelude = ./Lib/External/Prelude.partial.dhall

let Role = ./Lib/Role/Enum.partial.dhall

let Role/enabled = ./Lib/Role/enabled.partial.dhall

let Role/toMetadata = ./Lib/Role/toMetadata.partial.dhall

let Role/toText = ./codegen/Lib/Role/toText.partial.dhall

let Role/values = ./codegen/Lib/Role/values.partial.dhall

let assertRolesDependencies =
      let dependencies =
            External/Prelude.List.concat
              Role
              ( External/Prelude.List.filterMap
                  Role
                  (List Role)
                  ( \(role : Role) ->
                      if    Role/enabled role
                      then  Some (Role/toMetadata role).dependencies
                      else  None (List Role)
                  )
                  Role/values
              )

      in    assert
          : External/Prelude.List.all Role Role/enabled dependencies === True

let assertRolesConflicts =
      let conflicts =
            External/Prelude.List.concat
              Role
              ( External/Prelude.List.filterMap
                  Role
                  (List Role)
                  ( \(role : Role) ->
                      if    Role/enabled role
                      then  Some (Role/toMetadata role).conflicts
                      else  None (List Role)
                  )
                  Role/values
              )

      in    assert
          : External/Prelude.List.any Role Role/enabled conflicts === False

in  [ External/Ansible.Play::{
      , hosts = "all"
      , gather_facts = Some True
      , become = Some True
      , roles = Some
          ( External/Prelude.List.filterMap
              Role
              Text
              ( \(role : Role) ->
                  if    Role/enabled role
                  then  Some (Role/toText role)
                  else  None Text
              )
              Role/values
          )
      }
    ]
