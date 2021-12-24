let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/copyFiles = ../../../Lib/TaskPool/copyFiles.partial.dhall

let TaskPool/createDirectories =
      ../../../Lib/TaskPool/createDirectories.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some (TaskPool/createDirectories [ Directory/toText Directory.Ssh ])
          , Some
              ( TaskPool/executeCommands
                  Shell.Default
                  [ "test -f ${Directory/toText
                                 Directory.Ssh}/id_rsa || ssh-keygen -b 4096 -t rsa -f ${Directory/toText
                                                                                           Directory.Ssh}/id_rsa -N \"\""
                  ]
              )
          , Some
              ( TaskPool/copyFiles
                  [ External/Prelude.Map.keyValue
                      (List Text)
                      (Directory/toText Directory.Ssh)
                      [ "config" ]
                  ]
              )
          ]
      )
