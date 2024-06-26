let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

let Permission = ../../../Lib/Permission/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/copyFiles
          ( Some
              PermissionMode::{
              , user = [ Permission.Read, Permission.Write, Permission.Execute ]
              , group = [] : List Permission
              , other = [] : List Permission
              }
          )
          [ External/Prelude.Map.keyValue
              (List Text)
              (Directory/toText Directory.Xrandr)
              [ "default-desktop.sh"
              , "default-laptop.py"
              , "dock-only.py"
              , "laptop-dock.py"
              ]
          ]
      )
