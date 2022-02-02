let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let TaskPool/become = ../../../Lib/TaskPool/become.partial.dhall

let TaskPool/concat = ../../../Lib/TaskPool/concat.partial.dhall

let TaskPool/createDirectories =
      ../../../Lib/TaskPool/createDirectories.partial.dhall

let TaskPool/executeCommands =
      ../../../Lib/TaskPool/executeCommands.partial.dhall

let Privilege = ../../../Lib/Privilege/Enum.partial.dhall

let Shell = ../../../Lib/Shell/Enum.partial.dhall

in  TaskPool/become
      Privilege.User
      ( TaskPool/concat
          [ Some (TaskPool/createDirectories [ Directory/toText Directory.Lua ])
          , Some
              ( TaskPool/executeCommands
                  (Some Shell.Zsh)
                  [ "luarocks install --local --server=https://luarocks.org/dev luaformatter"
                  , "luarocks install --local fennel"
                  , "lua-format --dump-config > ${Directory/toText
                                                    Directory.Lua}/config.yaml"
                  ]
              )
          ]
      )
