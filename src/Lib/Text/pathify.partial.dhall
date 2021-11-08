let External/Prelude = ../External/Prelude.partial.dhall

let pathify
    : Text -> Text
    = \(path : Text) -> External/Prelude.Text.replace "//" "/" path

in  pathify
