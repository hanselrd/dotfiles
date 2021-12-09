let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let env = ../../../../build/environment.dhall

let PackageManager/toText = ../../../Lib/PackageManager/toText.partial.dhall

in  [ External/Ansible.Task::{
      , name = Some "Install packages"
      , package = Some External/Ansible.Package::{
        , name = "{{ item }}"
        , state = "present"
        }
      , loop = Some "{{ packages }}"
      , vars = Some
          ( External/Ansible.Vars.object
              ( toMap
                  { packages =
                      External/Ansible.Vars.array
                        ( External/Prelude.List.map
                            Text
                            External/Ansible.Vars.Type
                            External/Ansible.Vars.string
                            [ "a", "b", "c" ]
                        )
                  }
              )
          )
      }
    , External/Ansible.Task::{
      , name = Some "Remove packages"
      , package = Some External/Ansible.Package::{
        , name = "{{ item }}"
        , state = "absent"
        }
      , loop = Some "{{ packages }}"
      , vars = Some
          ( External/Ansible.Vars.object
              ( toMap
                  { packages =
                      External/Ansible.Vars.array
                        ( External/Prelude.List.map
                            Text
                            External/Ansible.Vars.Type
                            External/Ansible.Vars.string
                            [ "a", "b", "c" ]
                        )
                  }
              )
          )
      }
    , let packageManagerText = PackageManager/toText env.package_manager

      in  External/Ansible.Task::{
          , name = Some "Include ${packageManagerText} tasks"
          , include_tasks = Some External/Ansible.IncludeTasks::{
            , file = Some "${packageManagerText}.yml"
            }
          }
    ]
