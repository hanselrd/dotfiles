let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some
              ( TaskPool/copyFiles
                  (None PermissionMode.Type)
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Gtk1)
                      [ ".gtkrc-2.0" ]
                  ]
              )
          , Some
              ( TaskPool/copyFiles
                  (None PermissionMode.Type)
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Gtk2)
                      [ "settings.ini" ]
                  ]
              )
          ]
      )
