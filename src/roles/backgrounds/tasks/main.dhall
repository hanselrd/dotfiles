let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Background = ../../../Lib/Background/Enum.partial.dhall

let Background/toText = ../../../codegen/Lib/Background/toText.partial.dhall

let Background/values = ../../../codegen/Lib/Background/values.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let PermissionMode = ../../../Lib/PermissionMode/Record.partial.dhall

in  TaskPool/copyFiles
      (None PermissionMode.Type)
      [ External/Prelude.Map.keyValue
          (List Text)
          (Directory/toText Directory.Background)
          ( External/Prelude.List.map
              Background
              Text
              Background/toText
              Background/values
          )
      ]
