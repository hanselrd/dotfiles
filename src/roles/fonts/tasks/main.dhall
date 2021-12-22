let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Font = ../../../Lib/Font/Enum.partial.dhall

let Font/toText = ../../../codegen/Lib/Font/toText.partial.dhall

let Font/values = ../../../codegen/Lib/Font/values.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/unarchiveFiles = ../../../Lib/TaskPool/unarchiveFiles.partial.dhall

in  TaskPool/unarchiveFiles
      [ External/Prelude.Map.keyValue
          (List Text)
          (Directory/toText Directory.Font)
          (External/Prelude.List.map Font Text Font/toText Font/values)
      ]
