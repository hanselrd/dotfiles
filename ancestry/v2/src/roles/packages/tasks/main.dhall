let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let PackageManager/toText =
      ../../../codegen/Lib/PackageManager/toText.partial.dhall

let env = ../../../codegen/environment.partial.dhall

let packageManagerText = PackageManager/toText env.package_manager

in  [ External/Ansible.Task::{
      , name = Some "Include ${packageManagerText} tasks"
      , include_tasks = Some External/Ansible.IncludeTasks::{
        , file = Some "${packageManagerText}.yml"
        }
      }
    ]
