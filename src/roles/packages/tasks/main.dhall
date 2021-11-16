let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let PackageManager = ../../../Lib/PackageManager/Enum.partial.dhall

let PackageManager/toText = ../../../Lib/PackageManager/toText.partial.dhall

in  External/Prelude.List.map
      PackageManager
      External/Ansible.Task.Type
      ( \(packageManager : PackageManager) ->
          let packageManagerText = PackageManager/toText packageManager

          in  External/Ansible.Task::{
              , name = Some "Include ${packageManagerText} tasks"
              , include_tasks = Some External/Ansible.IncludeTasks::{
                , file = Some "${packageManagerText}.yml"
                }
              , when = Some "ansible_pkg_mgr == \"${packageManagerText}\""
              }
      )
      [ PackageManager.Pacman, PackageManager.Dnf, PackageManager.Apt ]
