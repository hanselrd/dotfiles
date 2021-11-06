let Role = ./Role.partial.dhall

in  { Type =
        { role : Role
        , enabled : Bool
        , dependencies : List Role
        , conflicts : List Role
        }
    , default = { dependencies = [] : List Role, conflicts = [] : List Role }
    }
