let External/Ansible = ./Lib/External/Ansible.partial.dhall

let External/Prelude = ./Lib/External/Prelude.partial.dhall

let Role = ./Lib/Role/Enum.partial.dhall

let RoleMeta = ./Lib/Role/EnumMeta.partial.dhall

let Role/Config = ./Lib/Role/Config/Record.partial.dhall

let Enum/toText = ./Lib/Enum/toText.partial.dhall

let Enum/equal = ./Lib/Enum/equal.partial.dhall

let Role/toMetadata = ./Lib/Role/toMetadata.partial.dhall

let Role/Config/sort = ./Lib/Role/Config/sort.partial.dhall

let env = ../build/environment.dhall

let assertRolesDependencies =
      let dependencies =
            External/Prelude.List.concatMap
              Role/Config.Type
              Role
              ( \(roleConfig : Role/Config.Type) ->
                  if    roleConfig.enabled
                  then  (Role/toMetadata roleConfig.role).dependencies
                  else  [] : List Role
              )
              env.roles

      in    assert
          :     External/Prelude.Bool.and
                  ( External/Prelude.List.map
                      Role/Config.Type
                      Bool
                      ( \(roleConfig : Role/Config.Type) ->
                          let role = roleConfig.role

                          in  External/Prelude.Bool.and
                                ( External/Prelude.List.map
                                    Role
                                    Bool
                                    ( \(depRole : Role) ->
                                        if    Enum/equal
                                                Role
                                                RoleMeta
                                                role
                                                depRole
                                        then  roleConfig.enabled
                                        else  True
                                    )
                                    dependencies
                                )
                      )
                      env.roles
                  )
            ===  True

let assertRolesConflicts =
      let conflicts =
            External/Prelude.List.concatMap
              Role/Config.Type
              Role
              ( \(roleConfig : Role/Config.Type) ->
                  if    roleConfig.enabled
                  then  (Role/toMetadata roleConfig.role).conflicts
                  else  [] : List Role
              )
              env.roles

      in    assert
          :     External/Prelude.Bool.or
                  ( External/Prelude.List.map
                      Role/Config.Type
                      Bool
                      ( \(roleConfig : Role/Config.Type) ->
                          let role = roleConfig.role

                          in  if    roleConfig.enabled
                              then  External/Prelude.Bool.or
                                      ( External/Prelude.List.map
                                          Role
                                          Bool
                                          ( \(conflictRole : Role) ->
                                              Enum/equal
                                                Role
                                                RoleMeta
                                                role
                                                conflictRole
                                          )
                                          conflicts
                                      )
                              else  False
                      )
                      env.roles
                  )
            ===  False

let addIncludeRoleTask =
      \(roleConfig : Role/Config.Type) ->
        let roleText = Enum/toText Role RoleMeta roleConfig.role

        in  if    roleConfig.enabled
            then  Some
                    External/Ansible.Task::{
                    , name = Some "Include ${roleText} role"
                    , include_role = Some External/Ansible.IncludeRole::{
                      , name = roleText
                      }
                    }
            else  None External/Ansible.Task.Type

in  [ External/Ansible.Play::{
      , hosts = "all"
      , gather_facts = Some True
      , become = Some True
      , tasks = Some
          ( External/Prelude.List.concat
              External/Ansible.Task.Type
              [ [ External/Ansible.Task::{
                  , name = Some "Create user directories"
                  , become = Some True
                  , become_user = Some env.user
                  , file = Some External/Ansible.File::{
                    , path = "{{ item }}"
                    , state = Some External/Ansible.File.state.directory
                    }
                  , loop = Some "{{ directories }}"
                  , vars = Some
                      ( External/Ansible.Vars.object
                          ( toMap
                              { directories =
                                  External/Ansible.Vars.array
                                    ( External/Prelude.List.map
                                        Text
                                        External/Ansible.Vars.Type
                                        External/Ansible.Vars.string
                                        [ env.user_cache_dir
                                        , env.user_config_dir
                                        , env.user_home_dir
                                        , env.user_root_dir
                                        , env.user_temp_dir
                                        ]
                                    )
                              }
                          )
                      )
                  }
                ]
              , External/Prelude.List.unpackOptionals
                  External/Ansible.Task.Type
                  ( External/Prelude.List.map
                      Role/Config.Type
                      (Optional External/Ansible.Task.Type)
                      addIncludeRoleTask
                      (Role/Config/sort env.roles)
                  )
              ]
          )
      }
    ]
