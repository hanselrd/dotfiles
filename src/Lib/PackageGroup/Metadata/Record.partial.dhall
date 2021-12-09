let Package = ../../Package/Record.partial.dhall

in  { Type = { present : List Package.Type, absent : List Package.Type }
    , default =
      { present = [] : List Package.Type, absent = [] : List Package.Type }
    }
