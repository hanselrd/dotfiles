let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/executeCommands
          (None Shell)
          [ "luarocks install --server=https://luarocks.org/dev luaformatter"
          , "luarocks install fennel"
          ]
      )
