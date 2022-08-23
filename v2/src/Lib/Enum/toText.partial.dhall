let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let toText
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> Text
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(value : a) ->
        External/Prelude.Text.default
          ( External/Prelude.List.head
              Text
              ( External/Prelude.List.filterMap
                  (EnumMeta a).Type
                  Text
                  ( \(enumMeta : (EnumMeta a).Type) ->
                      if enumMeta.equal value then enumMeta.text else None Text
                  )
                  enumMetas
              )
          )

in  toText
