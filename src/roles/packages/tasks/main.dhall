let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let env = ../../../../build/environment.dhall

let PackageManager = ../../../Lib/PackageManager/Enum.partial.dhall

let PackageManagerMeta = ../../../Lib/PackageManager/EnumMeta.partial.dhall

let Enum/toText = ../../../Lib/Enum/toText.partial.dhall

let packageManagerText =
      Enum/toText PackageManager PackageManagerMeta env.package_manager

in  [ External/Ansible.Task::{
      , name = Some "Include ${packageManagerText} tasks"
      , include_tasks = Some External/Ansible.IncludeTasks::{
        , file = Some "${packageManagerText}.yml"
        }
      }
    ]
