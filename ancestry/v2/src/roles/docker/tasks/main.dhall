let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let PackageManager = ../../../Lib/PackageManager/Enum.partial.dhall

let PackageManager/equal =
      ../../../codegen/Lib/PackageManager/equal.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

let TaskPool = ../../../Lib/TaskPool/Alias.partial.dhall

let TaskPool/build = ../../../Lib/TaskPool/build.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/build
      [ if        Role/enabled Role.Systemd
              &&  PackageManager/equal env.package_manager PackageManager.Pacman
        then  Some
                External/Ansible.Task::{
                , name = Some "Enable systemd unit"
                , systemd = Some External/Ansible.Systemd::{
                  , name = Some "docker.service"
                  , scope = Some External/Ansible.Systemd.scope.system
                  , enabled = Some True
                  , daemon_reload = Some True
                  , force = Some True
                  }
                }
        else  None TaskPool.Entry
      ]
