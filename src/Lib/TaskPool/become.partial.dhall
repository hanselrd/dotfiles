let External/Ansible = ../External/Ansible.partial.dhall

let External/Prelude = ../External/Prelude.partial.dhall

let Privilege = ../Privilege/Enum.partial.dhall

let Privilege/toText = ../../codegen/Lib/Privilege/toText.partial.dhall

let become
    : Privilege ->
      List External/Ansible.Task.Type ->
        List External/Ansible.Task.Type
    = \(privilege : Privilege) ->
      \(taskPool : List External/Ansible.Task.Type) ->
        External/Prelude.List.map
          External/Ansible.Task.Type
          External/Ansible.Task.Type
          ( \(task : External/Ansible.Task.Type) ->
                  task
              //  { become = Some True
                  , become_user = Some (Privilege/toText privilege)
                  }
          )
          taskPool

in  become
