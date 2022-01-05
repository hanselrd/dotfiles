let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/executeCommands
          (Some Shell.Zsh)
          [ "pip install --upgrade --user pipenv black cmakelang"
          , "pip install --user {{ role_path }}/files"
          ]
      )
