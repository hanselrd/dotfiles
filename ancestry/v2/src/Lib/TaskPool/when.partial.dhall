let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let TaskPool = ./Alias.partial.dhall

let when
    : TaskPool.Entry -> Text -> Text -> TaskPool.Type -> TaskPool.Type
    = \(task : TaskPool.Entry) ->
      \(register : Text) ->
      \(when : Text) ->
      \(taskPool : TaskPool.Type) ->
          [ task // { register = Some register }
          , External/Ansible.Task::{
            , debug = Some External/Ansible.Debug::{ var = Some register }
            }
          ]
        # External/Prelude.List.map
            TaskPool.Entry
            TaskPool.Entry
            (\(task : TaskPool.Entry) -> task // { when = Some when })
            taskPool

in  when
