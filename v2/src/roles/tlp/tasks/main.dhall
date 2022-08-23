let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let PackageManager = ../../../Lib/PackageManager/Enum.partial.dhall

let PackageManager/equal =
      ../../../codegen/Lib/PackageManager/equal.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Role/enabled = ../../../Lib/Role/enabled.partial.dhall

let TaskPool = ../../../Lib/TaskPool/Alias.partial.dhall

let TaskPool/build = ../../../Lib/TaskPool/build.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/concat
      [ Some
          ( TaskPool/copyFiles
              (None PermissionMode.Type)
              [ External/Prelude.Map.keyValue
                  (List Text)
                  (Directory/toText Directory.Tlp)
                  [ "tlp.conf" ]
              ]
          )
      , if        Role/enabled Role.Systemd
              &&  PackageManager/equal env.package_manager PackageManager.Pacman
        then  Some
                ( TaskPool/build
                    [ Some
                        External/Ansible.Task::{
                        , name = Some "Enable systemd unit"
                        , systemd = Some External/Ansible.Systemd::{
                          , name = Some "tlp.service"
                          , scope = Some External/Ansible.Systemd.scope.system
                          , enabled = Some True
                          , daemon_reload = Some True
                          , force = Some True
                          }
                        }
                    ]
                )
        else  None TaskPool.Type
      , if        Role/enabled Role.Systemd
              &&  PackageManager/equal env.package_manager PackageManager.Pacman
        then  Some
                ( TaskPool/build
                    [ Some
                        External/Ansible.Task::{
                        , name = Some "Mask systemd unit(s)"
                        , systemd = Some External/Ansible.Systemd::{
                          , name = Some "{{ item }}"
                          , scope = Some External/Ansible.Systemd.scope.system
                          , masked = Some True
                          , daemon_reload = Some True
                          , force = Some True
                          }
                        , loop = Some "{{ units }}"
                        , vars = Some
                            ( External/Ansible.Vars.object
                                ( toMap
                                    { units =
                                        External/Ansible.Vars.array
                                          ( External/Prelude.List.map
                                              Text
                                              External/Ansible.Vars.Type
                                              External/Ansible.Vars.string
                                              [ "systemd-rfkill.service"
                                              , "systemd-rfkill.socket"
                                              ]
                                          )
                                    }
                                )
                            )
                        }
                    ]
                )
        else  None TaskPool.Type
      ]
