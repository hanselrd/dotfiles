let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Configuration = ../../../Lib/Configuration/Enum.partial.dhall

let Configuration/equal = ../../../codegen/Lib/Configuration/equal.partial.dhall

let TaskPool = ../../../Lib/TaskPool/Alias.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  TaskPool/concat
      [ if    External/Prelude.Bool.not
                (Configuration/equal env.configuration Configuration.Remote)
        then  Some
                ( TaskPool/executeCommands
                    (None Shell)
                    [ "pip install {{ role_path }}/files" ]
                )
        else  None TaskPool.Type
      , Some
          ( TaskPool/become
              Privilege.User
              ( TaskPool/executeCommands
                  (None Shell)
                  [ "pip install --upgrade --user pipenv black cmakelang csvkit updog"
                  , "pip install --user {{ role_path }}/files"
                  ]
              )
          )
      ]
