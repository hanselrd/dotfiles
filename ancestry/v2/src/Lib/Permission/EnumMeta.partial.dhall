let External/Prelude = ../External/Prelude.partial.dhall

let Permission = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let default = { Read = False, Write = False, Execute = False }

let meta =
      { Read = (EnumMeta Permission)::{
        , value = Permission.Read
        , sort = Some 1
        , text = Some "r"
        , equal =
            \(permission : Permission) ->
              merge (default // { Read = True }) permission
        }
      , Write = (EnumMeta Permission)::{
        , value = Permission.Write
        , sort = Some 2
        , text = Some "w"
        , equal =
            \(permission : Permission) ->
              merge (default // { Write = True }) permission
        }
      , Execute = (EnumMeta Permission)::{
        , value = Permission.Execute
        , sort = Some 3
        , text = Some "x"
        , equal =
            \(permission : Permission) ->
              merge (default // { Execute = True }) permission
        }
      }

let validate = assert : merge meta Permission.Read === meta.Read

in  External/Prelude.Map.values Text (EnumMeta Permission).Type (toMap meta)
