let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      (TaskPool/executeCommands Shell.Zsh [ "nvm install --lts --latest-npm" ])
