let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Prelude/Text/pathify = ../../../Lib/Prelude/Text/pathify.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/copyFiles
          [ External/Prelude.Map.keyValue
              (List Text)
              (Prelude/Text/pathify "${env.user_config_dir}/wal/templates")
              [ "colors.dhall" ]
          ]
      )