let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

let Permission = ../../../Lib/Permission/Enum.partial.dhall

in  TaskPool/copyFiles
      ( Some
          PermissionMode::{
          , user = [ Permission.Read, Permission.Write, Permission.Execute ]
          , group = [ Permission.Read, Permission.Execute ]
          , other = [ Permission.Read, Permission.Execute ]
          }
      )
      [ External/Prelude.Map.keyValue
          (List Text)
          (Directory/toText Directory.Bin)
          [ "df_archive"
          , "df_backup"
          , "df_exit"
          , "df_launcher"
          , "df_passphrase"
          , "df_picom"
          , "df_power_now"
          , "df_redshift"
          , "df_xrandr"
          ]
      ]
