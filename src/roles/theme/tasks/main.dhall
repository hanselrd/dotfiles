let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Prelude/Text/pathify = ../../../Lib/Prelude/Text/pathify.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/copyFiles
      [ External/Prelude.Map.keyValue
          (List Text)
          (Prelude/Text/pathify "${env.user_config_dir}/wal/templates")
          [ "colors.dhall" ]
      ]
