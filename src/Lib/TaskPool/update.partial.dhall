let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Updater =
      { become : Optional Bool
      , become_user : Optional Text
      , when : Optional Text
      }

let update
    : Updater ->
      List External/Ansible.Task.Type ->
        List External/Ansible.Task.Type
    = \(updater : Updater) ->
      \(taskPool : List External/Ansible.Task.Type) ->
        External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          (\(task : External/Ansible.Task.Type) -> task // updater)
          taskPool

in  update
