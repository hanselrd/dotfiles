let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/executeCommands
          (Some Shell.Zsh)
          [ "npm install -g elm elm-format elm-test @elm-tooling/elm-language-server"
          , "npm update -g"
          ]
      )
