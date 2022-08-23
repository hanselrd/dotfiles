let Permission = ../Permission/Enum.partial.dhall

in  { Type =
        { user : List Permission
        , group : List Permission
        , other : List Permission
        }
    , default =
      { user = [ Permission.Read, Permission.Write ]
      , group = [ Permission.Read ]
      , other = [ Permission.Read ]
      }
    }
