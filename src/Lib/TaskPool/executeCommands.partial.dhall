let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let executeCommands
    : List Text -> List External/Ansible.Task.Type
    = \(commands : List Text) ->
        [ External/Ansible.Task::{
          , name = Some "Execute command(s)"
          , shell = Some External/Ansible.Shell::{
            , cmd = Some (External/Prelude.Text.concatSep "\n" commands)
            }
          , register = Some "output"
          }
        , External/Ansible.Task::{
          , debug = Some External/Ansible.Debug::{
            , var = Some "output.stdout_lines"
            }
          }
        ]

in  executeCommands
