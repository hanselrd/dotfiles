let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let env = ../../../build/environment.dhall

let executeCommands
    : List Text -> Bool -> List External/Ansible.Task.Type
    = \(commands : List Text) ->
      \(useZsh : Bool) ->
        [ External/Ansible.Task::{
          , name = Some "Execute command(s)"
          , shell = Some External/Ansible.Shell::{
            , cmd = Some "{{ item }}"
            , executable = if useZsh then Some "/usr/bin/zsh" else None Text
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
                                External/Ansible.Vars.string
                                (   ( if    useZsh
                                      then  [ ". ${env.user_home_dir}/.zshrc" ]
                                      else  [] : List Text
                                    )
                                  # commands
                                )
                            )
                      }
                  )
              )
          }
        ]

in  executeCommands
