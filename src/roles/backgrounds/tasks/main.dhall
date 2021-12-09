let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Background = ../../../Lib/Background/Enum.partial.dhall

let BackgroundMeta = ../../../Lib/Background/EnumMeta.partial.dhall

let Enum/toText = ../../../Lib/Enum/toText.partial.dhall

let Enum/values = ../../../Lib/Enum/values.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let DirectoryMeta = ../../../Lib/Directory/EnumMeta.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

in  External/Prelude.List.concat
      External/Ansible.Task.Type
      [ TaskPool/copyFiles
          [ External/Prelude.Map.keyValue
              (List Text)
              (Enum/toText Directory DirectoryMeta Directory.Background)
              ( External/Prelude.List.map
                  Background
                  Text
                  (Enum/toText Background BackgroundMeta)
                  (Enum/values Background BackgroundMeta)
              )
          ]
      ]
