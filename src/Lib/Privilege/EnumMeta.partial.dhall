let External/Prelude = ../External/Prelude.partial.dhall

let Privilege = ./Enum.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let env = ../../codegen/environment.partial.dhall

let default = { Root = False, User = False }

let meta =
      { Root = (EnumMeta Privilege)::{
        , value = Privilege.Root
        , text = Some "root"
        , equal =
            \(privilege : Privilege) ->
              merge (default // { Root = True }) privilege
        }
      , User = (EnumMeta Privilege)::{
        , value = Privilege.User
        , text = Some env.user
        , equal =
            \(privilege : Privilege) ->
              merge (default // { User = True }) privilege
        }
      }

let validate = assert : merge meta Privilege.Root === meta.Root

in  External/Prelude.Map.values Text (EnumMeta Privilege).Type (toMap meta)
