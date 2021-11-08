let Role/Enum = ./Enum.partial.dhall

in  { Type = { dependencies : List Role/Enum, conflicts : List Role/Enum }
    , default =
      { dependencies = [] : List Role/Enum, conflicts = [] : List Role/Enum }
    }
