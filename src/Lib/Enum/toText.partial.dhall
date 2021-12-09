let External/Prelude = ../External/Prelude.partial.dhall

let EnumMeta = ../EnumMeta/Record.partial.dhall

let toText
    : forall (a : Type) -> List (EnumMeta a).Type -> a -> Text
    = \(a : Type) ->
      \(enumMetas : List (EnumMeta a).Type) ->
      \(value : a) ->
        merge
          { Some =
              \(enumMeta : (EnumMeta a).Type) ->
                merge { Some = \(text : Text) -> text, None = "" } enumMeta.text
          , None = ""
          }
          ( External/Prelude.List.head
              (EnumMeta a).Type
              ( External/Prelude.List.filter
                  (EnumMeta a).Type
                  (\(enumMeta : (EnumMeta a).Type) -> enumMeta.equal value)
                  enumMetas
              )
          )

in  toText
