let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Shell = ../Shell/Enum.partial.dhall

let Shell/toText = ../../codegen/Lib/Shell/toText.partial.dhall

let TaskPool = ./Alias.partial.dhall

let TaskPool/build = ./build.partial.dhall

let executeCommands
    : Optional Shell -> List Text -> TaskPool.Type
    = \(shell : Optional Shell) ->
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
                          External/Prelude.Optional.map
                            Shell
                            Text
                            ( \(shell : Shell) ->
                                "/usr/bin/${Shell/toText shell}"
                            )
                            shell
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
                                                  External/Prelude.Optional.default
                                                    Text
                                                    command
                                                    ( External/Prelude.Optional.map
                                                        Shell
                                                        Text
                                                        ( \(shell : Shell) ->
                                                            "/usr/bin/${Shell/toText
                                                                          shell} -ic \"${command}\""
                                                        )
                                                        shell
                                                    )
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
