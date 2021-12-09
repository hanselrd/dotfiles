let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Role = ../Role/Enum.partial.dhall

let Role/toText = ../Role/toText.partial.dhall

let executeCommands
    : Role -> List Text -> List External/Ansible.Task.Type
    = \(role : Role) ->
      \(commands : List Text) ->
        let roleText = Role/toText role

        in  [ External/Ansible.Task::{
              , name = Some "Execute ${roleText} command(s)"
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
