let External/Ansible = ../../../Lib/External/Ansible.partial.dhall

let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Role = ../../../Lib/Role/Enum.partial.dhall

let Background = ../../../Lib/Background/Enum.partial.dhall

let Background/toText = ../../../Lib/Background/toText.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../Lib/Directory/toText.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

in  External/Prelude.List.concat
      External/Ansible.Task.Type
      [ TaskPool/copyFiles
          Role.Backgrounds
          [ External/Prelude.Map.keyValue
              (List Text)
              (Directory/toText Directory.Background)
              ( External/Prelude.List.map
                  Background
                  Text
                  Background/toText
                  [ Background.One
                  , Background.Two
                  , Background.Three
                  , Background.Four
                  , Background.Five
                  , Background.Six
                  , Background.Seven
                  , Background.Eight
                  , Background.Nine
                  ]
              )
          ]
      ]
