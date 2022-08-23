let External/Prelude = ../External/Prelude.partial.dhall

let Privilege = ../Privilege/Enum.partial.dhall

let Privilege/toText = ../../codegen/Lib/Privilege/toText.partial.dhall

let TaskPool = ./Alias.partial.dhall

let become
    : Privilege -> TaskPool.Type -> TaskPool.Type
    = \(privilege : Privilege) ->
        External/Prelude.List.map
          TaskPool.Entry
          TaskPool.Entry
          ( \(task : TaskPool.Entry) ->
                  task
              //  { become = Some True
                  , become_user = Some (Privilege/toText privilege)
                  }
          )

in  become
