let Ansible =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-ansible/0.2.2/package.dhall
        sha256:030d7d1b16172afde44843c6e950fcc3382a6653269e36a27ca1d06d75a631ff

let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/package.dhall
        sha256:46c48bba5eee7807a872bbf6c3cb6ee6c2ec9498de3543c5dcc7dd950e43999d

let Role = ./Role.partial.dhall

let RoleConfig = ./RoleConfig.partial.dhall

let Role/toText = ./Role/toText.partial.dhall

let Role/equal = ./Role/equal.partial.dhall

let env = (../build/environment.dhall).default

let assertRolesDependencies =
      let dependencies =
            Prelude.List.concatMap
              RoleConfig.Type
              Role
              ( \(roleConfig : RoleConfig.Type) ->
                  if    roleConfig.enabled
                  then  roleConfig.dependencies
                  else  [] : List Role
              )
              env.roles

      in    assert
          :     Prelude.Bool.and
                  ( Prelude.List.map
                      RoleConfig.Type
                      Bool
                      ( \(roleConfig : RoleConfig.Type) ->
                          let role = roleConfig.role

                          in  Prelude.Bool.and
                                ( Prelude.List.map
                                    Role
                                    Bool
                                    ( \(depRole : Role) ->
                                        if    Role/equal role depRole
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
            Prelude.List.concatMap
              RoleConfig.Type
              Role
              ( \(roleConfig : RoleConfig.Type) ->
                  if    roleConfig.enabled
                  then  roleConfig.conflicts
                  else  [] : List Role
              )
              env.roles

      in    assert
          :     Prelude.Bool.or
                  ( Prelude.List.map
                      RoleConfig.Type
                      Bool
                      ( \(roleConfig : RoleConfig.Type) ->
                          let role = roleConfig.role

                          in  if    roleConfig.enabled
                              then  Prelude.Bool.or
                                      ( Prelude.List.map
                                          Role
                                          Bool
                                          ( \(conflictRole : Role) ->
                                              Role/equal role conflictRole
                                          )
                                          conflicts
                                      )
                              else  False
                      )
                      env.roles
                  )
            ===  False

let addIncludeRoleTask =
      \(roleConfig : RoleConfig.Type) ->
        let roleText = Role/toText roleConfig.role

        in  if    roleConfig.enabled
            then  Some
                    Ansible.Task::{
                    , name = Some "Include ${roleText} role"
                    , include_role = Some Ansible.IncludeRole::{
                      , name = roleText
                      }
                    }
            else  None Ansible.Task.Type

in  [ Ansible.Play::{
      , hosts = "all"
      , gather_facts = Some True
      , become = Some True
      , tasks = Some
          ( Prelude.List.concat
              Ansible.Task.Type
              [ [ Ansible.Task::{
                  , name = Some "Create user directories"
                  , become = Some True
                  , become_user = Some env.user
                  , file = Some Ansible.File::{
                    , path = "{{ item }}"
                    , state = Some Ansible.File.state.directory
                    }
                  , loop = Some "{{ directories }}"
                  , vars = Some
                      ( Ansible.Vars.object
                          ( toMap
                              { directories =
                                  Ansible.Vars.array
                                    [ Ansible.Vars.string env.user_cache_dir
                                    , Ansible.Vars.string env.user_config_dir
                                    , Ansible.Vars.string env.user_home_dir
                                    , Ansible.Vars.string env.user_root_dir
                                    , Ansible.Vars.string env.user_temp_dir
                                    ]
                              }
                          )
                      )
                  }
                ]
              , Prelude.List.unpackOptionals
                  Ansible.Task.Type
                  ( Prelude.List.map
                      RoleConfig.Type
                      (Optional Ansible.Task.Type)
                      addIncludeRoleTask
                      env.roles
                  )
              ]
          )
      }
    ]
