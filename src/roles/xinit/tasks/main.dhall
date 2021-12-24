let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/copyFiles
          [ External/Prelude.Map.keyValue
              (List Text)
              (Directory/toText Directory.Xinit)
              [ ".xinitrc" ]
          ]
      )
