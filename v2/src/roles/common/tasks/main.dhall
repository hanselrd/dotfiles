let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Prelude = ../../../Lib/Prelude.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/become
      Privilege.User
      [ External/Ansible.Task::{
        , name = Some "Create user directories"
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
                              ( External/Prelude.Function.compose
                                  Text
                                  Text
                                  External/Ansible.Vars.Type
                                  Prelude.Text.pathify
                                  External/Ansible.Vars.string
                              )
                              [ env.user_home_dir
                              , env.user_cache_dir
                              , env.user_config_dir
                              , env.user_root_dir
                              , env.user_temp_dir
                              ]
                          )
                    }
                )
            )
        }
      ]
