let Role = ./Enum.partial.dhall

in  { Type = { dependencies : List Role, conflicts : List Role }
    , default = { dependencies = [] : List Role, conflicts = [] : List Role }
    }
