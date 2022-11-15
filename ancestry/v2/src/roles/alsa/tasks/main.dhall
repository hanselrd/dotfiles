let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

in  TaskPool/copyFiles
      (None PermissionMode.Type)
      [ External/Prelude.Map.keyValue
          (List Text)
          (Directory/toText Directory.Alsa)
          [ "alsa-base.conf" ]
      ]
