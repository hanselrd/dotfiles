let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/build = ../../../Lib/TaskPool/build.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

let Permission = ../../../Lib/Permission/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some
              ( TaskPool/copyFiles
                  (None PermissionMode.Type)
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Systemd1)
                      [ "bootstrapd.service" ]
                  ]
              )
          , Some
              ( TaskPool/copyFiles
                  ( Some
                      PermissionMode::{
                      , user =
                        [ Permission.Read
                        , Permission.Write
                        , Permission.Execute
                        ]
                      , group = [ Permission.Read, Permission.Execute ]
                      , other = [ Permission.Read, Permission.Execute ]
                      }
                  )
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Systemd2)
                      [ "bootstrapd.sh", "bootstrapd_pre.sh" ]
                  ]
              )
          , Some
              ( TaskPool/build
                  [ Some
                      External/Ansible.Task::{
                      , name = Some "Enable systemd unit"
                      , systemd = Some External/Ansible.Systemd::{
                        , name = Some "bootstrapd.service"
                        , scope = Some External/Ansible.Systemd.scope.user
                        , enabled = Some True
                        , daemon_reload = Some True
                        , force = Some True
                        }
                      }
                  ]
              )
          ]
      )
