let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Shell = ../Shell/Enum.partial.dhall

let Shell/equal = ../../codegen/Lib/Shell/equal.partial.dhall

let TaskPool = ./Alias.partial.dhall

let TaskPool/build = ./build.partial.dhall

let executeCommands
    : Shell -> List Text -> TaskPool.Type
    = \(shell : Shell) ->
      \(commands : List Text) ->
        TaskPool/build
          [ if    External/Prelude.Bool.not
                    (External/Prelude.List.null Text commands)
            then  Some
                    External/Ansible.Task::{
                    , name = Some "Execute command(s)"
                    , shell = Some External/Ansible.Shell::{
                      , cmd = Some "{{ item }}"
                      , executable =
                          if    Shell/equal shell Shell.Zsh
                          then  Some "/usr/bin/zsh"
                          else  None Text
                      }
                    , loop = Some "{{ commands }}"
                    , vars = Some
                        ( External/Ansible.Vars.object
                            ( toMap
                                { commands =
                                    External/Ansible.Vars.array
                                      ( External/Prelude.List.map
                                          Text
                                          External/Ansible.Vars.Type
                                          ( External/Prelude.Function.compose
                                              Text
                                              Text
                                              External/Ansible.Vars.Type
                                              ( \(command : Text) ->
                                                  if    Shell/equal
                                                          shell
                                                          Shell.Zsh
                                                  then  "/usr/bin/zsh -ic \"${command}\""
                                                  else  command
                                              )
                                              External/Ansible.Vars.string
                                          )
                                          commands
                                      )
                                }
                            )
                        )
                    }
            else  None TaskPool.Entry
          ]

in  executeCommands
