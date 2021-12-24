let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

in  TaskPool/copyFiles
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
